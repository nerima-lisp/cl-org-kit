(in-package #:cl-org-kit/test)

(defun table-plot-test-find-table (node)
  (or (when (eq (node-type node) :table) node)
      (some #'table-plot-test-find-table (node-children node))))

(defun table-plot-test-capture-error (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (error (condition) condition)))

(defun table-plot-test-plan-entry (plan path)
  (find path (write-plan-entries plan) :key #'write-plan-entry-path
                                         :test #'string=))

(defun table-plot-test-entry-content (plan path)
  (let ((entry (table-plot-test-plan-entry plan path)))
    (and entry (write-plan-entry-content entry))))

(defun table-plot-test-document (source &key limits)
  (let* ((document (parse-org source :limits (or limits (make-limits))))
         (table (table-plot-test-find-table (document-root document))))
    (values document table)))

(defun table-plot-test-oversized-document (source)
  (let* ((limits (make-limits :max-input-length 1))
         (document (org-kit::%make-document :text source :line-starts #(0)
                                            :limits limits))
         (row (make-node :table-row :properties '(:type :standard)
                         :children
                         (list (make-node :table-cell :properties '(:value "x"))
                               (make-node :table-cell :properties '(:value "y")))))
         (table (make-node :table :properties '(:type :org) :children (list row))))
    (setf (document-root document) (make-node :org-data :children (list table)))
    (values document table)))

(describe "Org plot write plans"
  (it "matches the Org 9.8.10 script and data oracle without writing files"
    (let* ((source (format nil
                           "#+PLOT: title:\"Quarterly\" ind:1 deps:(2 3) with:lines set:\"grid\"~%| Month | Sales | Cost |~%|-------+-------+------|~%| Jan | 1.5 | 2 |~%| Feb | 3 | words |~%"))
           (document (parse-org source))
           (table (table-plot-test-find-table (document-root document)))
           (expected-script
             (format nil
                     "reset~%set term GNUTERM ~%~%set title 'Quarterly'~%set grid~%set datafile separator \"\\t\"~%plot 'plot/data.tsv' using 1:2 with lines title 'Sales',\\~%    'plot/data.tsv' using 1:3 with lines title 'Cost'"))
           (expected-data
             (format nil "\"Jan\"~C1.5~C2~%\"Feb\"~C3~C\"words\""
                     #\Tab #\Tab #\Tab #\Tab))
           (plan (org-kit::table-plot-write-plan
                  document table :script-path "plot/script.gp"
                  :data-path "plot/data.tsv"))
           (fixtures (asdf:system-relative-pathname
                      "cl-org-kit" "t/fixtures/table/plot/")))
      (with-soft-assertions
        (expect (write-plan-p plan) :to-be t)
        (expect (length (write-plan-entries plan)) :to-be 2)
        (expect (table-plot-test-entry-content plan "plot/script.gp")
                :to-equal expected-script)
        (expect (table-plot-test-entry-content plan "plot/data.tsv")
                :to-equal expected-data)
        (expect (document-text document) :to-equal source)
        (expect (probe-file (merge-pathnames "unwritten-script.gp" fixtures))
                :to-be nil)
        (expect (probe-file (merge-pathnames "unwritten-data.tsv" fixtures))
                :to-be nil))))

  (it "requires supplied contents for the script named by PLOT"
    (multiple-value-bind (document table)
        (table-plot-test-document
         (format nil "#+PLOT: script:\"custom.gp\"~%| x | y |~%|---+---|~%| 1 | 2 |~%"))
      (let ((condition
              (table-plot-test-capture-error
               (lambda ()
                 (org-kit::table-plot-write-plan
                  document table :script-path "script.gp" :data-path "data.tsv")))))
        (with-soft-assertions
          (expect (typep condition 'org-table-error) :to-be t)
          (when (typep condition 'org-table-error)
            (expect (org-table-error-reason condition)
                    :to-be :missing-plot-script-content))))))

  (it "reports an unknown option by name"
    (multiple-value-bind (document table)
        (table-plot-test-document
         (format nil "#+PLOT: bogus:yes~%| x | y |~%|---+---|~%| 1 | 2 |~%"))
      (let ((condition
              (table-plot-test-capture-error
               (lambda ()
                 (org-kit::table-plot-write-plan
                  document table :script-path "script.gp" :data-path "data.tsv")))))
        (with-soft-assertions
          (expect (typep condition 'org-table-error) :to-be t)
          (when (typep condition 'org-table-error)
            (expect (org-table-error-reason condition) :to-be :unknown-plot-option)
            (expect (search "bogus" (org-kit-error-message condition))
                    :to-be-truthy))))))

  (it "returns radar data, script, and setup as three plan entries"
    (multiple-value-bind (document table)
        (table-plot-test-document
         (format nil "#+PLOT: type:radar~%| Axis | Design | QA |~%|------+--------+----|~%| Speed | 1 | 10 |~%| Cost | 2 | 20 |~%"))
      (let* ((plan (org-kit::table-plot-write-plan
                    document table :script-path "plot/radar.gp"
                    :data-path "plot/radar.tsv"
                    :radar-path "plot/radar-data.dat"))
             (expected-data
               (format nil "\"Speed\"~C1~C10~%\"Cost\"~C2~C20"
                       #\Tab #\Tab #\Tab #\Tab))
             (expected-radar
               (format nil
                       "# Data~%$Data <<HEREHAVESOMEDATA~%\"Axis\" \"Design\" \"QA\"~%\"Speed\" 1 10~%\"Cost\" 2 20~%\"Speed\" 1 10~%HEREHAVESOMEDATA~%HeaderLines = 1~%~%# Settings for scale and offset adjustments~%# axis min max tics axisLabelXoff axisLabelYoff~%$Settings <<EOD~%\"Speed\" 1.0 10.0 9~%\"Cost\" 2.0 20.0 9~%\"Speed\" 1.0 10.0 9~%EOD~%")))
        (with-soft-assertions
          (expect (length (write-plan-entries plan)) :to-be 3)
          (expect (table-plot-test-entry-content plan "plot/radar.tsv")
                  :to-equal expected-data)
          (expect (table-plot-test-entry-content plan "plot/radar-data.dat")
                  :to-equal expected-radar)
          (expect (search "plot/radar-data.dat"
                          (table-plot-test-entry-content plan "plot/radar.gp"))
                  :to-be-truthy)))))
    (it "uses Org's row-normalized radar tick count"
    (multiple-value-bind (document table)
        (table-plot-test-document
         (format nil "#+PLOT: type:radar~%| Axis | A | B |~%|------+---+---|~%| First | 0 | 6 |~%| Second | 0 | 12 |~%| Third | 0 | 14 |~%"))
      (let* ((plan (org-kit::table-plot-write-plan
                    document table :script-path "plot/three.gp"
                    :data-path "plot/three.tsv"
                    :radar-path "plot/three.dat"))
             (script (table-plot-test-entry-content plan "plot/three.gp"))
             (radar (table-plot-test-entry-content plan "plot/three.dat")))
        (with-soft-assertions
          (expect (search "TicCount = 6" script) :to-be-truthy)
          (expect (search "\"First\" 0.0 6.0 6" radar) :to-be-truthy)
          (expect (search "\"Second\" 0.0 12.0 6" radar) :to-be-truthy)
          (expect (search "\"Third\" 0.0 14.0 6" radar) :to-be-truthy)))))

  (it "parses plot options and applies them to the generated script"
    (let* ((source
             (format nil
                     "#+PLOT: title:\"Quarterly \\\"net\\\"\" ind:1 deps:(2,3) type:2d with:linespoints file:\"charts/sales.svg\" set:\"grid\" labels:(\"Region\" \"Net Sales\")~%| North | 10 | 12 |~%| South | 5 | 8 |~%"))
           (document (parse-org source))
           (table (table-plot-test-find-table (document-root document)))
           (plan (org-kit::table-plot-write-plan
                  document table :script-path #p"plot/script.gp"
                  :data-path "plot/data.tsv" :base-directory "reports/"))
           (script (table-plot-test-entry-content plan "plot/script.gp")))
      (with-soft-assertions
        (expect (search "set term svg " script) :to-be-truthy)
        (expect (search "set output 'reports/charts/sales.svg'" script)
                :to-be-truthy)
        (expect (search "set title 'Quarterly \"net\"'" script)
                :to-be-truthy)
        (expect (search "set grid" script) :to-be-truthy)
        (expect (search "using 1:2 with linespoints title 'Net Sales'" script)
                :to-be-truthy)
        (expect (search "using 1:3 with linespoints title '3'" script)
                :to-be-truthy))))

  (it "quotes data fields while preserving numeric spelling and empty cells"
    (multiple-value-bind (document table)
        (table-plot-test-document
         (format nil
                 "| Empty | Code | Plus | Invalid | Quoted |~%|-------+------+------+---------+--------|~%| | 001 | +2 | -x | Say \"yes\" |~%"))
      (let ((plan (org-kit::table-plot-write-plan
                   document table :script-path "script.gp" :data-path "data.tsv")))
        (with-soft-assertions
          (expect (table-plot-test-entry-content plan "data.tsv")
                  :to-equal (format nil "~C001~C+2~C\"-x\"~C\"Say \"\"yes\"\"\""
                                    #\Tab #\Tab #\Tab #\Tab))))))

  (it "renders grid and three-dimensional data plans"
    (multiple-value-bind (document table)
        (table-plot-test-document
         (format nil
                 "#+PLOT: type:grid deps:(2,3)~%| Name | A | B |~%|------+---+---|~%| East | 1 | nope |~%| West | 2 | 4 |~%"))
      (let* ((plan (org-kit::table-plot-write-plan
                    document table :script-path "grid.gp" :data-path "grid.tsv"))
             (data (table-plot-test-entry-content plan "grid.tsv"))
             (script (table-plot-test-entry-content plan "grid.gp")))
        (with-soft-assertions
          (expect data :to-equal
                  (format nil
                          "0  0.500000  1.000000~%0  1.500000  1.000000~%0  1.500000  2.000000~%0  2.500000  2.000000~%~%1  0.500000  0.000000~%1  1.500000  0.000000~%1  1.500000  4.000000~%1  2.500000  4.000000~%~%"))
          (expect (search "splot 'grid.tsv' with pm3d title ''" script)
                  :to-be-truthy)))
    (multiple-value-bind (document table)
        (table-plot-test-document
         (format nil "#+PLOT: type:3d~%| Name | A | B |~%|------+---+---|~%| East | 1 | 2 |~%"))
      (let* ((plan (org-kit::table-plot-write-plan
                    document table :script-path "three.gp" :data-path "three.tsv"))
             (script (table-plot-test-entry-content plan "three.gp")))
        (with-soft-assertions
          (expect (search "splot 'three.tsv' matrix with lines title ''" script)
                  :to-be-truthy)
          (expect (table-plot-test-entry-content plan "three.tsv")
                  :to-equal
                  (format nil "0  0.500000  0.000000~%0  1.500000  0.000000~%~%1  0.500000  1.000000~%1  1.500000  1.000000~%~%")))))))

  (it "replaces every datafile token in a supplied script and resolves output paths"
    (multiple-value-bind (document table)
        (table-plot-test-document
         (format nil
                 "#+PLOT: file:\"/tmp/chart.svg\" script:\"load $datafile; load $datafile\"~%| x | y |~%|---+---|~%| 1 | 2 |~%"))
      (let* ((plan (org-kit::table-plot-write-plan
                    document table :script-path "script.gp" :data-path "data.tsv"
                    :script-content "load $datafile; load $datafile"
                    :base-directory "ignored/"))
             (script (table-plot-test-entry-content plan "script.gp")))
        (with-soft-assertions
          (expect (search "set term svg " script) :to-be-truthy)
          (expect (search "set output '/tmp/chart.svg'" script) :to-be-truthy)
          (expect (search "load data.tsv; load data.tsv" script) :to-be-truthy)
          (expect (search "$datafile" script) :to-be nil))))
    (with-soft-assertions
      (expect (org-kit::table-plot-replace-datafile "unchanged" "data.tsv")
              :to-equal "unchanged")
      (expect (org-kit::table-plot-gnuplot-file-path "chart" #p"reports/")
              :to-equal "reports/chart")
      (expect (org-kit::table-plot-gnuplot-file-path "chart" "reports")
              :to-equal "reports/chart")
      (expect (org-kit::table-plot-file-extension "chart.svg") :to-equal "svg")
      (expect (org-kit::table-plot-file-extension "dir.with.dot/chart")
              :to-be nil)
      (expect (org-kit::table-plot-option-string 12) :to-equal "12")
      (expect (org-kit::table-plot-option-string 'lines) :to-equal "lines")
      (expect (org-kit::table-plot-option-string '(a b)) :to-equal "(A B)")))

  (it "rejects malformed plot options, unsupported types, and missing radar inputs"
    (dolist (case (list
                   (list (format nil "#+PLOT: title:~%| x | y |~%|---+---|~%| 1 | 2 |~%")
                         'org-table-error :invalid-plot-option)
                   (list (format nil "#+PLOT: malformed~%| x | y |~%|---+---|~%| 1 | 2 |~%")
                         'org-table-error :unknown-plot-option)
                   (list (format nil "#+PLOT: deps:(2 3~%| x | y |~%|---+---|~%| 1 | 2 |~%")
                         'org-table-error :invalid-plot-option)
                   (list (format nil "#+PLOT: type:pie~%| x | y |~%|---+---|~%| 1 | 2 |~%")
                         'org-table-error :unsupported-plot-type)
                   (list (format nil "#+PLOT: type:radar~%| x | y |~%|---+---|~%| 1 | 2 |~%")
                         'org-table-error :missing-radar-path)))
      (multiple-value-bind (document table)
          (table-plot-test-document (first case))
        (let ((condition
                (table-plot-test-capture-error
                 (lambda ()
                   (org-kit::table-plot-write-plan
                    document table :script-path "script.gp" :data-path "data.tsv")))))
          (expect (typep condition (second case)) :to-be t)
          (when (typep condition 'org-table-error)
            (expect (org-table-error-reason condition) :to-be (third case))))))
    (multiple-value-bind (document table)
        (table-plot-test-document
         (format nil "| x | y |~%|---+---|~%| 1 | 2 |~%"))
      (let ((condition
              (table-plot-test-capture-error
               (lambda ()
                 (org-kit::table-plot-write-plan
                  document table :script-path nil :data-path "data.tsv")))))
        (expect (typep condition 'type-error) :to-be t)))
    (let ((condition
            (table-plot-test-capture-error
             (lambda ()
               (org-kit::table-plot-option-value-end
                (format nil "\"unfinished~%value\"") 0 nil)))))
      (with-soft-assertions
        (expect (typep condition 'org-table-error) :to-be t)
        (when (typep condition 'org-table-error)
          (expect (org-table-error-reason condition) :to-be :invalid-plot-option)))))

  (it "formats radar values and selects normalized tick frequencies"
    (with-soft-assertions
      (expect (org-kit::table-plot-prime-factors 1) :to-equal nil)
      (expect (org-kit::table-plot-prime-factors 12) :to-equal '(3 2 2))
      (expect (org-kit::table-plot-real-string 4) :to-equal "4")
      (expect (org-kit::table-plot-real-string 4.0d0) :to-equal "4.0")
      (expect (org-kit::table-plot-real-string 4.125d0) :to-equal "4.125")
      (expect (org-kit::table-plot-normalized-frequencies nil) :to-be nil)
      (expect (org-kit::table-plot-normalized-frequencies '(2 2 3))
              :to-equal '((2 . 0.6666666666666666d0) (3 . 0.3333333333333333d0)))
      (expect (org-kit::table-plot-merge-frequencies
               '((2 . 0.5d0) (3 . 0.5d0)) '((3 . 0.25d0) (5 . 0.75d0)))
              :to-equal '((2 . 0.5d0) (3 . 0.75d0) (5 . 0.75d0)))
      (expect (org-kit::table-plot-nice-frequency-pick nil) :to-be nil)
      (expect (org-kit::table-plot-nice-frequency-pick '((2 . 1d0)))
              :to-equal '(2))
      (expect (org-kit::table-plot-nice-frequency-pick
               '((2 . 0.8d0) (3 . 0.2d0)))
              :to-equal '(2 2))
      (expect (org-kit::table-plot-nice-frequency-pick
               '((2 . 0.6d0) (3 . 0.4d0)))
              :to-equal '(2 3))
      (expect (org-kit::table-plot-nice-frequency-pick
               '((1 . 1/3) (2 . 1/3) (3 . 1/3)))
              :to-equal '(3 2 1)))
    (multiple-value-bind (document table)
        (table-plot-test-document
         (format nil "#+PLOT: type:radar~%| Flat | 5 | 10 |~%"))
      (let ((condition
              (table-plot-test-capture-error
               (lambda ()
                 (org-kit::table-plot-write-plan
                  document table :script-path "flat.gp" :data-path "flat.tsv"
                  :radar-path "flat.dat")))))
        (with-soft-assertions
          (expect (typep condition 'org-table-error) :to-be t)
          (when (typep condition 'org-table-error)
            (expect (org-table-error-reason condition) :to-be :missing-plot-labels))))))
  (it "covers plot parser and text writer boundaries"
      (let* ((text "(\"line\\n\\t\")")
             (end (org-kit::table-plot-option-value-end text 0 nil))
             (values (org-kit::table-plot-parse-option-list text 0 end))
             (unterminated
              (table-plot-test-capture-error
               (lambda ()
                 (org-kit::table-plot-option-value-end "\"unfinished" 0 nil))))
             (grid-output
              (with-output-to-string (stream)
                (org-kit::table-plot-grid-data
                 '(nil) nil (make-limits) stream
                 (org-kit::make-table-plot-budget :limits (make-limits))
                 "data.tsv")))
             (frequency
              (org-kit::table-plot-nice-frequency-pick
               '((2 . 1d0) (3 . 0.2d0) (5 . 0.04d0))))
             (symbol 'cl-codec-kit:string-size-in-octets)
             (original (symbol-function symbol))
             (write-condition nil))
        (unwind-protect
            (progn
              (setf (symbol-function symbol)
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (error "encoder failed")))
              (setf write-condition
                    (table-plot-test-capture-error
                     (lambda ()
                       (with-output-to-string (stream)
                         (org-kit::table-plot-write-text
                          stream "value"
                          (org-kit::make-table-plot-budget :limits (make-limits))
                          "data.tsv")))))
              (with-soft-assertions
               (expect end :to-be (length text))
               (expect values :to-equal (list (format nil "line~%~C" #\Tab)))
               (expect (typep unterminated 'org-table-error) :to-be t)
               (when (typep unterminated 'org-table-error)
                 (expect (org-table-error-reason unterminated)
                         :to-be :invalid-plot-option))
               (expect grid-output :to-equal "")
               (expect frequency :to-equal '(2 2))
               (expect (typep write-condition 'org-write-plan-error) :to-be t)
               (when (typep write-condition 'org-write-plan-error)
                 (expect (org-write-plan-error-reason write-condition)
                         :to-be :invalid-utf-8))))
          (setf (symbol-function symbol) original))))

  (it "enforces document, output, entry, byte, and path limits"
      (let ((source (format nil "| x | y |~%|---+---|~%| 1 | 2 |~%")))
        (multiple-value-bind (document table)
            (table-plot-test-oversized-document source)
          (let ((condition
                 (table-plot-test-capture-error
                  (lambda ()
                    (org-kit::table-plot-write-plan
                     document table :script-path "script.gp"
                     :data-path "data.tsv")))))
            (expect (typep condition 'org-resource-limit-error) :to-be t)
            (when (typep condition 'org-resource-limit-error)
              (expect (org-resource-limit-error-limit-name condition)
                      :to-be :max-input-length))))
        (dolist (case (list
                       (list :max-output-length 1)
                       (list :max-output-octets 1)
                       (list :max-write-plan-entries 1)
                       (list :max-write-plan-bytes 1)))
          (let* ((limit-name (first case))
                 (limits (apply #'make-limits (list limit-name (second case))))
                 (document (parse-org source :limits limits))
                 (table (table-plot-test-find-table (document-root document)))
                 (condition
                  (table-plot-test-capture-error
                   (lambda ()
                     (org-kit::table-plot-write-plan
                      document table :script-path "script.gp"
                      :data-path "data.tsv")))))
            (expect (typep condition 'org-resource-limit-error) :to-be t)
            (when (typep condition 'org-resource-limit-error)
              (expect (org-resource-limit-error-limit-name condition)
                      :to-be limit-name))))
        (dolist (path-case
                 (list (list "long/script.gp" :max-write-plan-path-bytes 4)
                       (list "deep/nested/script.gp" :max-write-plan-path-depth 1)))
          (let* ((limits
                  (apply (symbol-function (find-symbol "MAKE-LIMITS" "ORG-KIT"))
                         (rest path-case)))
                 (document (parse-org source :limits limits))
                 (table (table-plot-test-find-table (document-root document)))
                 (condition
                  (table-plot-test-capture-error
                   (lambda ()
                     (org-kit::table-plot-write-plan
                      document table :script-path (first path-case)
                      :data-path "data.tsv")))))
            (expect (typep condition (quote org-resource-limit-error)) :to-be t)
            (when (typep condition (quote org-resource-limit-error))
              (expect (org-resource-limit-error-limit-name condition)
                      :to-be (second path-case)))))))
  (it "covers plot option, path, dependency, and radar branches"
    (let* ((quote-characters (list (code-char 39) (code-char 39)))
           (quoted-atom
             (org-kit::table-plot-parse-option-atom "\"label\"" 0 7))
           (script-options
             (org-kit::table-plot-read-options "script:\"custom.gp\"" nil))
           (rows (list (list "Group" 1 2 3)))
           (filtered-lines
             (org-kit::table-plot-plot-lines
              "2d" rows
              (list :ind 0 :deps (list 2) :with "lines")
              "data.tsv" "radar.dat"))
           (all-lines
             (org-kit::table-plot-plot-lines
              "2d" rows
              (list :ind 0 :deps nil :with "lines")
              "data.tsv" "radar.dat"))
           (radar-data
             (with-output-to-string (stream)
               (org-kit::table-plot-write-radar-setup
                (list (list "Flat" "5" "5") (list "Empty"))
                (list "A" "B") nil (make-limits) 0 stream
                (org-kit::make-table-plot-budget :limits (make-limits))
                "radar.dat")))
           (flat-ticks
             (org-kit::table-plot-radar-tick-count
              (list (list "Flat" "5" "5")) (make-limits)))
           (range-ticks
             (org-kit::table-plot-radar-tick-count
              (list (list "Range" "0" "4")) (make-limits)))
           (zero-ticks-script
             (org-kit::table-plot-radar-script nil (list :ticks 0) "radar.dat"))
           (four-ticks-script
             (org-kit::table-plot-radar-script nil (list :ticks 4) "radar.dat"))
           (tick-vector (format nil "~%    ~C++~C u" (first quote-characters)
                                (second quote-characters))))
      (with-soft-assertions
        (expect (org-kit::table-plot-option-value-end "((a))" 0 nil)
                :to-be 5)
        (expect quoted-atom :to-equal "label")
        (expect (org-kit::table-plot-parse-option-value "(1 2)" 0 5)
                :to-equal (list 1 2))
        (progn
          (expect (org-kit::table-plot-parse-option-value "value" 0 5)
                  :to-equal "value")
          (expect (org-kit::table-plot-parse-option-value "x" 1 1)
                  :to-equal ""))
        (expect (getf script-options :script) :to-equal "custom.gp")
        (progn
          (expect (org-kit::table-plot-gnuplot-file-path nil "reports")
                  :to-be nil)
          (expect (org-kit::table-plot-gnuplot-file-path "" "reports")
                  :to-equal "reports/")
          (expect (org-kit::table-plot-gnuplot-file-path "chart" "")
                  :to-equal "/chart"))
        (expect (org-kit::table-plot-gnuplot-file-path "/tmp/chart" "reports")
                :to-equal "/tmp/chart")
        (expect (org-kit::table-plot-gnuplot-file-path "chart" nil)
                :to-equal "chart")
        (expect (org-kit::table-plot-gnuplot-file-path "chart" "reports")
                :to-equal "reports/chart")
        (expect (org-kit::table-plot-gnuplot-file-path "chart" "reports/")
                :to-equal "reports/chart")
        (expect (org-kit::table-plot-label nil 0) :to-be 1)
        (progn
          (expect (org-kit::table-plot-label (list "Sales") 0)
                  :to-equal "Sales")
          (expect (org-kit::table-plot-label "Sales" 0) :to-be 1))
        (expect (length filtered-lines) :to-be 1)
        (expect (search "using 2" (first filtered-lines)) :to-be-truthy)
        (expect (length all-lines) :to-be 4)
        (expect (search "\"Flat\" 5 5" radar-data) :to-be-truthy)
        (expect (search "\"Empty\" 0.0 0.0" radar-data) :to-be-truthy)
        (expect flat-ticks :to-be 0)
        (expect (> range-ticks 0) :to-be t)
        (expect (org-kit::table-plot-real-string 2) :to-equal "2")
        (expect (org-kit::table-plot-real-string 3.5d0) :to-equal "3.5")
        (expect (org-kit::table-plot-real-string 5.0d0) :to-equal "5.0")
        (expect (search "TicCount = 2" zero-ticks-script) :to-be-truthy)
        (expect (search tick-vector zero-ticks-script) :to-be nil)
        (expect (search "TicCount = 4" four-ticks-script) :to-be-truthy)
        (expect (search tick-vector four-ticks-script) :to-be-truthy))))

)
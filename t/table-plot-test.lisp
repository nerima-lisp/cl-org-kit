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
                    :to-be-truthy)))))

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
      (multiple-value-bind (document table) (table-plot-test-document source)
        (dolist (path-case '(("long/script.gp" "data.tsv" :max-write-plan-path-bytes 4)
                             ("deep/nested/script.gp" "data.tsv" :max-write-plan-path-depth 1)))
          (let ((condition
                  (table-plot-test-capture-error
                   (lambda ()
                     (org-kit::table-plot-write-plan
                      document table :script-path (first path-case)
                      :data-path (second path-case)
                      :limits (apply #'make-limits
                                     (list (third path-case) (fourth path-case))))))))
            (expect (typep condition 'org-resource-limit-error) :to-be t)
            (when (typep condition 'org-resource-limit-error)
              (expect (org-resource-limit-error-limit-name condition)
                      :to-be (third path-case)))))))))
)

;;;; src/table-plot.lisp
(in-package #:org-kit)

(defstruct table-plot-budget limits (characters 0) (octets 0))

(defun table-plot-signal-error (target reason message)
  (error 'org-table-error
         :message message
         :target (edit-target-description target)
         :reason reason))

(defun table-plot-write-text (stream text budget path)
  (let ((limits (table-plot-budget-limits budget))
        (characters (+ (table-plot-budget-characters budget)
                       (length text)))
        (octets 0))
    (loop for start from 0 below (length text) by 4096
          for end = (min (length text) (+ start 4096))
          do (incf octets
                   (handler-case
                       (cl-codec-kit:string-size-in-octets
                        text :start start :end end :encoding :utf-8)
                     (error () (write-plan-error :invalid-utf-8 path))))
             (check-limit limits :max-output-octets
                          (+ (table-plot-budget-octets budget) octets))
             (check-limit limits :max-write-plan-bytes
                          (+ (table-plot-budget-octets budget) octets)))
    (check-limit limits :max-output-length characters)
    (setf (table-plot-budget-characters budget) characters
          (table-plot-budget-octets budget)
          (+ (table-plot-budget-octets budget) octets))
    (write-string text stream)))

(defun table-plot-path (path limits)
  (unless (or (stringp path) (pathnamep path))
    (error 'type-error :datum path :expected-type '(or string pathname)))
  (nth-value 0 (write-plan-normalized-path path limits)))

(defun table-plot-option-delimiter-p (character)
  (find character '(#\Space #\Tab #\Newline #\Return #\; #\, #\.)))

(defun table-plot-skip-option-delimiters (text position)
  (loop while (and (< position (length text))
                   (table-plot-option-delimiter-p (char text position)))
        do (incf position)
        finally (return position)))

(defun table-plot-option-value-end (text start target)
  (let ((first (char text start)))
    (cond
      ((char= first #\")
       (loop with escaped = nil
             for position from (1+ start) below (length text)
             for character = (char text position)
             do (cond (escaped (setf escaped nil))
                      ((char= character #\\) (setf escaped t))
                      ((char= character #\") (return (1+ position)))
                      ((char= character #\Newline)
                       (table-plot-signal-error target :invalid-plot-option
                                                "Unterminated quoted plot option")))
             finally (table-plot-signal-error target :invalid-plot-option
                                              "Unterminated quoted plot option")))
      ((char= first #\()
       (loop with depth = 0
             with quoted-p = nil
             with escaped-p = nil
             for position from start below (length text)
             for character = (char text position)
             do (cond
                  (escaped-p (setf escaped-p nil))
                  ((and quoted-p (char= character #\\)) (setf escaped-p t))
                  ((char= character #\") (setf quoted-p (not quoted-p)))
                  (quoted-p nil)
                  ((char= character #\() (incf depth))
                  ((char= character #\))
                   (decf depth)
                   (when (zerop depth) (return (1+ position))))
                  (t nil))
             finally (table-plot-signal-error target :invalid-plot-option
                                              "Unterminated list plot option")))
      (t (or (position-if #'table-plot-option-delimiter-p text :start start)
             (length text))))))

(defun table-plot-unescape-string (text start end)
  (with-output-to-string (stream)
    (loop for position from (1+ start) below (1- end)
          for character = (char text position)
          do (if (and (char= character #\\) (< (1+ position) (1- end)))
                 (progn
                   (incf position)
                   (write-char (case (char text position)
                                 (#\n #\Newline)
                                 (#\t #\Tab)
                                 (otherwise (char text position))) stream))
                 (write-char character stream)))))

(defun table-plot-parse-option-atom (text start end)
  (if (and (< start end) (char= (char text start) #\"))
      (table-plot-unescape-string text start end)
      (let* ((token (subseq text start end))
             (number (ignore-errors (parse-integer token :junk-allowed nil))))
        (or number token))))

(defun table-plot-parse-option-list (text start end)
  (let ((values nil)
        (position (1+ start))
        (limit (1- end)))
    (loop
      (setf position (table-plot-skip-option-delimiters text position))
      (when (>= position limit) (return))
      (let* ((value-end (table-plot-option-value-end text position nil))
             (bounded-end (min value-end limit)))
        (push (table-plot-parse-option-atom text position bounded-end) values)
        (setf position bounded-end)))
    (nreverse values)))

(defun table-plot-parse-option-value (text start end)
  (cond ((and (< start end) (char= (char text start) #\())
         (table-plot-parse-option-list text start end))
        (t (table-plot-parse-option-atom text start end))))

(defun table-plot-read-options (text target)
  (let ((options (list :type "2d" :with "lines" :ind 0 :sets nil))
        (position 0))
    (loop
      (setf position (table-plot-skip-option-delimiters text position))
      (when (>= position (length text)) (return options))
      (let* ((start position)
             (colon (position #\: text :start position))
             (key-end (or colon
                          (position-if #'table-plot-option-delimiter-p text
                                       :start position)
                          (length text)))
             (name (subseq text start key-end)))
        (unless (and colon (= colon key-end) (> (length name) 0))
          (table-plot-signal-error target :unknown-plot-option
                                   (format nil "Unknown plot option ~A" name)))
        (unless (member name '("title" "ind" "deps" "type" "with" "file"
                               "set" "labels" "script") :test #'string=)
          (table-plot-signal-error target :unknown-plot-option
                                   (format nil "Unknown plot option ~A" name)))
        (let* ((value-start (1+ colon))
               (value-start (table-plot-skip-option-delimiters text value-start))
               (value-end (if (< value-start (length text))
                              (table-plot-option-value-end text value-start target)
                              value-start)))
          (when (= value-start value-end)
            (table-plot-signal-error target :invalid-plot-option
                                     (format nil "Plot option ~A has no value" name)))
          (let ((value (table-plot-parse-option-value text value-start value-end)))
            (cond ((string= name "title") (setf (getf options :title) value))
                  ((string= name "ind") (setf (getf options :ind) value))
                  ((string= name "deps") (setf (getf options :deps) value))
                  ((string= name "type") (setf (getf options :type) value))
                  ((string= name "with") (setf (getf options :with) value))
                  ((string= name "file") (setf (getf options :file) value))
                  ((string= name "set") (push value (getf options :sets)))
                  ((string= name "labels") (setf (getf options :labels) value))
                  (t (setf (getf options :script) value)))
            (setf position value-end)))))))

(defun table-plot-option-string (value)
  (typecase value
    (string value)
    (integer (format nil "~D" value))
    (symbol (string-downcase (symbol-name value)))
    (t (princ-to-string value))))

(defun table-plot-rows (table)
  (loop for row in (node-children table)
        collect (if (eq (node-property row :type) :rule)
                    :hline
                    (loop for cell in (node-children row)
                          collect (node-property cell :value "")))))

(defun table-plot-prepare-rows (rows options)
  (if (and (cdr rows) (eq (cadr rows) :hline))
      (values (loop for row in (cddr rows) unless (eq row :hline) collect row)
              (car rows))
      (values (remove :hline rows :test #'eq)
              (getf options :labels))))

(defun table-plot-number (value limits)
  (let ((decimal (parse-safe-decimal value :limits limits)))
    (if decimal
        (float (safe-decimal-rational decimal) 1d0)
        0d0)))

(defun table-plot-float (value)
  (format nil "~,6F" value))

(defun table-plot-data-field (value)
  (cond ((zerop (length value)) "")
        ((and (plusp (length value))
              (or (digit-char-p (char value 0))
                  (member (char value 0) '(#\+ #\-)))
              (parse-safe-decimal value))
         value)
        (t (with-output-to-string (stream)
             (write-char #\" stream)
             (loop for character across value
                   do (when (char= character #\") (write-char #\" stream))
                      (write-char character stream))
             (write-char #\" stream)))))

(defun table-plot-write-data (rows stream budget path)
  (loop for row in rows
        for row-index from 0
        do (when (plusp row-index)
             (table-plot-write-text stream (string #\Newline) budget path))
           (loop for value in row
                 for column from 0
                 do (when (plusp column)
                      (table-plot-write-text stream (string #\Tab) budget path))
                    (table-plot-write-text stream
                                           (table-plot-data-field value)
                                           budget path))))

(defun table-plot-grid-data (rows options limits stream budget path)
  (let* ((ind (1- (or (getf options :ind) 0)))
         (width (length (first rows)))
         (deps (or (getf options :deps)
                   (loop for column below width collect column)))
         (deps (remove ind (mapcar (lambda (value) (1- value)) deps)))
         (selected (if (or deps (>= ind 0))
                       (mapcar (lambda (row)
                                 (loop for value in row
                                       for column from 0
                                       when (member column deps) collect value))
                               rows)
                       rows)))
    (loop for column below (length (first selected))
          do (progn
               (loop for row in selected
                     for row-index from 1
                     for value = (table-plot-number (nth column row) limits)
                     do (dolist (text
                                  (list (format nil "~D  ~A  ~A~%~D  ~A  ~A~%"
                                                column
                                                (table-plot-float (- row-index 0.5d0))
                                                (table-plot-float value)
                                                column
                                                (table-plot-float (+ row-index 0.5d0))
                                                (table-plot-float value))))
                          (table-plot-write-text stream text budget path)))
               (table-plot-write-text stream (string #\Newline) budget path)))))

(defun table-plot-gnuplot-file-path (file base-directory)
  (if (or (null file)
          (and (plusp (length file)) (char= (char file 0) #\/))
          (null base-directory))
      file
      (let* ((base (if (pathnamep base-directory)
                       (sb-ext:native-namestring base-directory)
                       base-directory))
             (separator (if (and (plusp (length base))
                                 (char= (char base (1- (length base))) #\/))
                            "" "/")))
        (concatenate 'string base separator file))))

(defun table-plot-file-extension (file)
  (when file
    (let* ((slash (position #\/ file :from-end t))
           (dot (position #\. file :from-end t)))
      (and dot (or (null slash) (> dot slash))
           (subseq file (1+ dot))))))

(defun table-plot-label (labels column)
  (or (and (listp labels) (nth column labels)) (1+ column)))

(defun table-plot-plot-lines (type rows options data-path radar-path)
  (let ((width (length (first rows)))
        (ind (getf options :ind))
        (deps (getf options :deps))
        (labels (getf options :labels))
        (with (table-plot-option-string (getf options :with))))
    (cond
      ((string= type "3d")
       (list (format nil "'~A' matrix with ~A title ''" data-path with)))
      ((string= type "grid")
       (list (format nil "'~A' with pm3d title ''" data-path)))
      ((string= type "radar")
       (list (table-plot-radar-script labels options radar-path)))
      (t
       (loop for column below width
             for column-number = (1+ column)
             unless (or (and ind (= column-number ind))
                        (and deps (not (member column-number deps))))
               collect (format nil "'~A' using ~A~D with ~A title '~A'"
                               data-path
                               (if (and ind (> ind 0)) (format nil "~D:" ind) "")
                               column-number with
                               (table-plot-option-string
                                (table-plot-label labels column))))))))

(defun table-plot-script (type rows options data-path radar-path base-directory
                          script-content script-content-p stream budget path)
  (let* ((file (table-plot-gnuplot-file-path (getf options :file)
                                              base-directory))
         (extension (table-plot-file-extension file))
         (plot-lines (table-plot-plot-lines type rows options data-path radar-path))
         (plot-command (cond ((member type '("3d" "grid") :test #'string=)
                              "splot")
                             ((string= type "radar") nil)
                             (t "plot")))
         (script (getf options :script)))
    (labels ((line (text)
               (table-plot-write-text stream (string #\Newline) budget path)
               (table-plot-write-text stream text budget path)))
      (table-plot-write-text stream "reset" budget path)
      (line (format nil "set term ~A " (or extension "GNUTERM")))
      (when file (line (format nil "set output '~A'" file)))
      (line "")
      (when (getf options :title)
        (line (format nil "set title '~A'" (table-plot-option-string
                                             (getf options :title)))))
      (dolist (setting (getf options :sets))
        (line (format nil "set ~A" (table-plot-option-string setting))))
      (line "set datafile separator \"\\t\"")
      (if script
          (progn
            (unless script-content-p
              (table-plot-signal-error path :missing-plot-script-content
                                       "The named plot script has no supplied content"))
            (line (table-plot-replace-datafile script-content data-path)))
          (line
           (with-output-to-string (plot-stream)
             (when plot-command
               (write-string plot-command plot-stream)
               (write-char #\Space plot-stream))
             (loop for item in plot-lines
                   for index from 0
                   do (when (plusp index)
                        (write-string ",\\
    " plot-stream))
                      (write-string item plot-stream))))))))

(defun table-plot-replace-datafile (text data-path)
  (with-output-to-string (stream)
    (loop with position = 0
          for match = (search "$datafile" text :start2 position)
          do (if match
                 (progn
                   (write-string text stream :start position :end match)
                   (write-string data-path stream)
                   (setf position (+ match (length "$datafile"))))
                 (progn
                   (write-string text stream :start position)
                   (return))))))

(defun table-plot-write-radar-setup (rows labels options limits ticks
                                          stream budget path)
  (declare (ignore options))
  (unless labels
    (table-plot-signal-error path :missing-plot-labels
                             "Radar plots require labels for the data columns"))
  (table-plot-write-text stream (format nil
                                        "~A~%~A~%"
                                        "# Data"
                                        "$Data <<HEREHAVESOMEDATA") budget path)
  (table-plot-write-text stream
                         (format nil "\"~{~A~^\" \"~}\"~%"
                                 (mapcar #'table-plot-option-string labels))
                         budget path)
  (let ((closed-rows (append rows (when rows (list (first rows))))))
    (dolist (row closed-rows)
      (table-plot-write-text
       stream
       (format nil "\"~A\"~{ ~A~}~%"
               (or (first row) "")
               (mapcar #'table-plot-option-string (rest row)))
       budget path)))
  (table-plot-write-text stream
                         (format nil
                                 "~A~%~A~%~%~A~%~A~%~A~%"
                                 "HEREHAVESOMEDATA"
                                 "HeaderLines = 1"
                                 "# Settings for scale and offset adjustments"
                                 "# axis min max tics axisLabelXoff axisLabelYoff"
                                 "$Settings <<EOD")
                         budget path)
  (dolist (row (append rows (when rows (list (first rows)))))
    (let* ((values (mapcar (lambda (value) (table-plot-number value limits))
                           (rest row)))
           (minimum (if values (reduce #'min values) 0d0))
           (maximum (if values (reduce #'max values) 0d0))
           (range (- maximum minimum))
           (factor (if (zerop range) 1d0
                     (expt 10d0 (ceiling (- 1 (log range 10d0))))))
           (nice-min (if (zerop range) minimum
                       (/ (float (floor (* minimum factor)) 1d0) factor)))
           (nice-max (if (zerop range) maximum
                       (/ (float (ceiling (* maximum factor)) 1d0) factor))))
      (table-plot-write-text
       stream
       (format nil "\"~A\" ~A ~A ~D~%"
               (or (first row) "")
               (table-plot-real-string nice-min)
               (table-plot-real-string nice-max)
               (if (zerop ticks) 2 ticks))
       budget path)))
  (table-plot-write-text stream (format nil "EOD~%") budget path))

(defun table-plot-normalized-frequencies (values)
  (let ((counts nil)
        (total (length values)))
    (dolist (value values)
      (let ((entry (assoc value counts)))
        (if entry
            (incf (cdr entry))
            (setf counts (append counts (list (cons value 1)))))))
    (when (plusp total)
      (mapcar (lambda (entry)
                (cons (car entry) (/ (float (cdr entry) 1d0) total)))
              counts))))

(defun table-plot-merge-frequencies (left right)
  (let ((keys (append (mapcar #'car left)
                      (remove-if (lambda (key) (assoc key left))
                                 (mapcar #'car right)))))
    (mapcar (lambda (key)
              (cons key (+ (or (cdr (assoc key left)) 0d0)
                           (or (cdr (assoc key right)) 0d0))))
            keys)))

(defun table-plot-nice-frequency-pick (frequencies)
  (case (length frequencies)
    (0 nil)
    (1 (list (caar frequencies)))
    (2 (if (>= (/ (cdar frequencies) (cdadr frequencies)) 3)
           (list (caar frequencies) (caar frequencies))
           (list (caar frequencies) (caadr frequencies))))
    (otherwise
     (let* ((total (reduce #'+ frequencies :key #'cdr))
            (normalized (mapcar (lambda (entry)
                                  (cons (car entry) (/ (cdr entry) total)))
                                frequencies))
            (first-factor (caar normalized))
            (second-factor (caadr normalized))
            (third-factor (car (third normalized)))
            (ratio-1-2 (/ (cdar normalized) (cdadr normalized)))
            (ratio-2-3 (/ (cdadr normalized) (cdr (third normalized))))
            (ratio-1-3 (* ratio-1-2 ratio-2-3))
            (picked (list first-factor)))
       (when (> ratio-1-2 4)
         (push first-factor picked))
       (when (and (< ratio-1-2 second-factor)
                  (< (* (reduce #'* picked) second-factor) 30))
         (push second-factor picked))
       (when (and (< ratio-1-3 third-factor)
                  (< (* (reduce #'* picked) third-factor) 30))
         (push third-factor picked))
       picked))))

(defun table-plot-radar-tick-count (rows limits)
  (let ((weighted-frequencies nil))
    (dolist (row rows)
      (let* ((values (mapcar (lambda (value) (table-plot-number value limits))
                             (rest row)))
             (minimum (if values (reduce #'min values) 0d0))
             (maximum (if values (reduce #'max values) 0d0))
             (range (- maximum minimum))
             (factor (if (zerop range) 1d0
                       (expt 10d0 (ceiling (- 1 (log range 10d0))))))
             (nice-min (if (zerop range) minimum
                         (/ (float (floor (* minimum factor)) 1d0) factor)))
             (nice-max (if (zerop range) maximum
                         (/ (float (ceiling (* maximum factor)) 1d0) factor)))
             (normalized (round (* factor (- nice-max nice-min))))
             (normalized (if (zerop (mod normalized 10))
                             (/ normalized 10)
                           normalized)))
        (setf weighted-frequencies
              (table-plot-merge-frequencies
               weighted-frequencies
               (table-plot-normalized-frequencies
                (table-plot-prime-factors normalized))))))
    (let* ((frequencies (stable-sort weighted-frequencies #'> :key #'cdr))
           (picked (table-plot-nice-frequency-pick frequencies)))
      (if picked (reduce #'* picked) 0))))
(defun table-plot-prime-factors (number)
  (let ((value number)
        (factor 2)
        (factors nil))
    (loop while (> value 1)
          do (if (zerop (mod value factor))
                 (progn (push factor factors) (setf value (/ value factor)))
                 (incf factor)))
    factors))
(defun table-plot-real-string (value)
  (if (integerp value)
      (format nil "~D" value)
      (let* ((text (format nil "~,8F" value))
             (trimmed (string-right-trim "0" text)))
        (if (char= (char trimmed (1- (length trimmed))) #\.)
            (concatenate 'string trimmed "0")
            trimmed))))
(defun table-plot-radar-script (labels options radar-path)
  (declare (ignore labels))
  (let* ((ticks (getf options :ticks))
         (lines
           (append
            (list
             "### spider plot/chart with gnuplot"
             "# also known as: radar chart, web chart, star chart, cobweb chart,"
             "#                radar plot,  web plot,  star plot,  cobweb plot,  etc. ..."
             "set datafile separator ' '"
             "set size square"
             "unset tics"
             "set angles degree"
             "set key bmargin center horizontal"
             "unset border"
             ""
             "# Load data and setup"
             (concatenate 'string "load \"" radar-path "\"")
             ""
             "# General settings"
             "DataColCount = words($Data[1])-1"
             "AxesCount = |$Data|-HeaderLines-1"
             "AngleOffset = 90"
             "Max = 1"
             "d=0.1*Max"
             "Direction = -1   # counterclockwise=1, clockwise = -1"
             ""
             "# Tic settings"
             (format nil "TicCount = ~D" (if (zerop ticks) 2 ticks))
             "TicOffset = 0.1"
             "TicValue(axis,i) = real(i)*(word($Settings[axis],3)-word($Settings[axis],2)) \\"
             (format nil "~C  / word($Settings[axis],4)+word($Settings[axis],2)" #\Tab)
             "TicLabelPosX(axis,i) = PosX(axis,i/TicCount) + PosY(axis, TicOffset)"
             "TicLabelPosY(axis,i) = PosY(axis,i/TicCount) - PosX(axis, TicOffset)"
             "TicLen = 0.03"
             "TicdX(axis,i) = 0.5*TicLen*cos(alpha(axis)-90)"
             "TicdY(axis,i) = 0.5*TicLen*sin(alpha(axis)-90)"
             ""
             "# Label"
             "LabOffset = 0.10"
             "LabX(axis) = PosX(axis+1,Max+2*d) + PosY(axis, LabOffset)"
             "LabY(axis) = PosY($0+1,Max+2*d)"
             ""
             "# Functions"
             "alpha(axis) = (axis-1)*Direction*360.0/AxesCount+AngleOffset"
             "PosX(axis,R) = R*cos(alpha(axis))"
             "PosY(axis,R) = R*sin(alpha(axis))"
             "Scale(axis,value) = real(value-word($Settings[axis],2))/(word($Settings[axis],3)-word($Settings[axis],2))"
             ""
             "# Spider settings"
             "set style arrow 1 dt 1 lw 1.0 @fgal head filled size 0.06,25     # style for axes"
             "set style arrow 2 dt 2 lw 0.5 @fgal nohead   # style for weblines"
             "set style arrow 3 dt 1 lw 1 @fgal nohead     # style for axis tics"
             "set samples AxesCount"
             "set isosamples TicCount"
             "set urange[1:AxesCount]"
             "set vrange[1:TicCount]"
             "set style fill transparent solid 0.2"
             ""
             "set xrange[-Max-4*d:Max+4*d]"
             "set yrange[-Max-4*d:Max+4*d]"
             "plot \\"
             "    '+' u (0):(0):(PosX($0,Max+d)):(PosY($0,Max+d)) w vec as 1 not, \\"
             "    $Data u (LabX($0)): \\"
             (format nil "~C(LabY($0)):1 every ::HeaderLines w labels center enhanced @fgt not, \\"
                      #\Tab)
             "    for [i=1:DataColCount] $Data u (PosX($0+1,Scale($0+1,column(i+1)))): \\"
             (format nil "~C(PosY($0+1,Scale($0+1,column(i+1)))) every ::HeaderLines w filledcurves lt i title word($Data[1],i+1), \\"
                      #\Tab))
            (if (zerop ticks)
                nil
                (list
                 "    '++' u (PosX($1,$2/TicCount)):(PosY($1,$2/TicCount)): \\"
                 (format nil "~C(PosX($1+1,$2/TicCount)-PosX($1,$2/TicCount)):  \\"
                          #\Tab)
                 (format nil "~C(PosY($1+1,$2/TicCount)-PosY($1,$2/TicCount)) w vec as 2 not, \\"
                          #\Tab)
                 "    '++' u (TicLabelPosX($1,$2)):(TicLabelPosY($1,$2)): \\"
                 (format nil "~C(sprintf('%g',TicValue($1,$2))) w labels font ',8' @fgat not"
                          #\Tab)))
            (list
             "#    '++' u (PosX($1,$2/TicCount)-TicdX($1,$2/TicCount)): \\"
             "#        (PosY($1,$2/TicCount)-TicdY($1,$2/TicCount)): \\"
             "#        (2*TicdX($1,$2/TicCount)):(2*TicdY($1,$2/TicCount)) \\"
             "#        w vec as 3 not, \\"
             "### end of code"))))
    (with-output-to-string (stream)
      (dolist (line lines)
        (write-string line stream)
        (write-char #\Newline stream)))))
(defun table-plot-write-plan (document target
                              &key script-path data-path radar-path
                                ((:script-content script-content) nil script-content-p)
                                base-directory)
  "Build a write plan for a table's plot script, data, and radar setup.
SCRIPT-PATH and DATA-PATH are required relative paths; RADAR-PATH is required
for radar plots. SCRIPT-CONTENT supplies the content named by a PLOT script
option, and BASE-DIRECTORY resolves a relative image path."
  (let* ((table (table-edit-resolve-table document target))
         (limits (document-limits document))
         (source (document-text document))
         (option-text (node-property table :plot ""))
         (options (table-plot-read-options option-text table))
         (type (table-plot-option-string (getf options :type)))
         (rows (table-plot-rows table))
         (script-path (table-plot-path script-path limits))
         (data-path (table-plot-path data-path limits))
         (radar-path (when (string= type "radar")
                       (unless radar-path
                         (table-plot-signal-error
                          table :missing-radar-path
                          "Radar plots require an auxiliary output path"))
                       (table-plot-path radar-path limits)))
         (budget (make-table-plot-budget :limits limits))
         (radar-ticks nil))
    (check-limit limits :max-input-length (length source))
    (unless (member type '("2d" "3d" "grid" "radar") :test #'string=)
      (table-plot-signal-error table :unsupported-plot-type
                               (format nil "Unsupported plot type ~A" type)))
    (multiple-value-bind (data-rows labels)
        (table-plot-prepare-rows rows options)
      (setf (getf options :labels) labels)
      (let* ((entry-count (if (string= type "radar") 3 2))
             (data-content
               (with-output-to-string (stream)
                 (if (member type '("3d" "grid") :test #'string=)
                     (table-plot-grid-data data-rows options limits stream budget
                                           data-path)
                     (table-plot-write-data data-rows stream budget data-path))))
             (entries (list (make-write-plan-entry data-path data-content))))
        (check-limit limits :max-write-plan-entries entry-count)
        (when (string= type "radar")
          (setf radar-ticks
                (table-plot-radar-tick-count data-rows limits)
                (getf options :ticks) radar-ticks)
          (let ((radar-content
                  (with-output-to-string (stream)
                    (table-plot-write-radar-setup
                     data-rows labels options limits radar-ticks stream budget
                     radar-path))))
            (setf entries
                  (append entries
                          (list (make-write-plan-entry radar-path radar-content))))))
        (let ((script-content-text
                (with-output-to-string (stream)
                  (table-plot-script
                   type data-rows options data-path radar-path base-directory
                   script-content script-content-p stream budget script-path))))
          (setf entries
                (append entries
                        (list (make-write-plan-entry script-path
                                                     script-content-text)))))
        (make-write-plan entries)))))

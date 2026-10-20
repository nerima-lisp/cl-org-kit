(in-package #:org-kit)
(defun table-radio-signal-error (target reason message)
  (table-align-signal-error target reason message))

(defun table-radio-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun table-radio-line-start (text offset)
  (1+ (or (position #\Newline text :end offset :from-end t) -1)))

(defun table-radio-line-content-end (text start)
  (let ((newline (position #\Newline text :start start)))
    (let ((end (or newline (length text))))
      (if (and (> end start) (char= (char text (1- end)) #\Return))
          (1- end)
          end))))

(defun table-radio-line-next (text start)
  (let ((newline (position #\Newline text :start start)))
    (if newline (1+ newline) (length text))))

(defun table-radio-line-spans (text)
  (loop with start = 0
        while (< start (length text))
        for end = (table-radio-line-content-end text start)
        for next = (table-radio-line-next text start)
        collect (list start end next)
        do (setf start next)))

(defun table-radio-trim-line (line)
  (string-trim '(#\Space #\Tab #\Return) line))

(defun table-radio-prefix-p (text prefix)
  (and (>= (length text) (length prefix))
       (string-equal text prefix :end1 (length prefix))))

(defun table-radio-string-equal-at-p (text start value)
  (and (<= (+ start (length value)) (length text))
       (string-equal text value :start1 start :end1 (+ start (length value)))))

(defun table-radio-next-word (text start)
  (loop with position = start
        while (and (< position (length text))
                   (table-radio-horizontal-space-p (char text position)))
        do (incf position)
        finally (return position)))

(defun table-radio-read-words (text target)
  (let ((position 0)
        (words nil))
    (loop while (< position (length text))
          do (setf position (table-radio-next-word text position))
             (when (< position (length text))
               (if (char= (char text position) #\")
                   (let ((stream (make-string-output-stream))
                         (closed-p nil))
                     (incf position)
                     (loop while (< position (length text))
                           for character = (char text position)
                           do (incf position)
                              (cond
                                ((char= character #\")
                                 (setf closed-p t)
                                 (return))
                                ((char= character #\\)
                                 (if (< position (length text))
                                     (progn
                                       (write-char (char text position) stream)
                                       (incf position))
                                     (table-radio-signal-error
                                      target :invalid-parameters
                                      "A generic parameter string ends with an escape")))
                                (t (write-char character stream))))
                     (unless closed-p
                       (table-radio-signal-error
                        target :invalid-parameters
                        "A generic parameter string is not closed"))
                     (push (list :string (get-output-stream-string stream)) words)
                     (when (and (< position (length text))
                                (not (table-radio-horizontal-space-p
                                      (char text position))))
                       (table-radio-signal-error
                        target :invalid-parameters
                        "Generic parameter strings must be separate tokens")))
                   (let ((start position))
                     (loop while (and (< position (length text))
                                      (not (table-radio-horizontal-space-p
                                            (char text position))))
                           do (incf position))
                     (push (list :atom (subseq text start position)) words)))))
    (nreverse words)))

(defun table-radio-parse-safe-atom (atom target)
  (cond
    ((string-equal atom "t") t)
    ((string-equal atom "nil") nil)
    ((and (plusp (length atom))
          (or (digit-char-p (char atom 0))
              (and (> (length atom) 1)
                   (char= (char atom 0) #\-)
                   (digit-char-p (char atom 1)))))
     (handler-case
         (let ((value (parse-integer atom)))
           (unless (string= atom (write-to-string value))
             (table-radio-signal-error
              target :invalid-parameters "Expected an integer literal"))
           value)
       (parse-error ()
         (table-radio-signal-error
          target :invalid-parameters "Expected an integer literal"))))
    (t (table-radio-signal-error
        target :invalid-parameters
        "Generic parameters accept only strings, integers, T, and NIL"))))

(defun table-radio-parameter-key (word target)
  (let* ((atom (second word))
         (name (and (eq (first word) :atom)
                    (> (length atom) 1)
                    (char= (char atom 0) #\:)
                    (string-downcase (subseq atom 1))))
         (entry (assoc name
                       '(("sep" . :sep) ("hsep" . :hsep) ("hline" . :hline)
                         ("splice" . :splice) ("skip" . :skip)
                         ("tstart" . :tstart) ("tend" . :tend)
                         ("lstart" . :lstart) ("lend" . :lend)
                         ("llstart" . :llstart) ("llend" . :llend)
                         ("hlstart" . :hlstart) ("hlend" . :hlend)
                         ("hllstart" . :hllstart) ("hllend" . :hllend)
                         ("fmt" . :fmt) ("hfmt" . :hfmt)
                         ("lfmt" . :lfmt) ("llfmt" . :llfmt)
                         ("hlfmt" . :hlfmt) ("hllfmt" . :hllfmt)
                         ("backend" . :backend) ("raw" . :raw)
                         ("booktabs" . :booktabs)
                         ("environment" . :environment)
                         ("columns" . :columns))
                       :test #'string=)))
    (or (cdr entry)
        (table-radio-signal-error
         target :invalid-parameters "Unsupported generic table parameter"))))

(defun table-radio-safe-parameters (words format target)
  (let ((parameters nil))
    (loop while words
          for key-word = (pop words)
          for value-word = (pop words)
          do (unless value-word
               (table-radio-signal-error
                target :invalid-parameters
                "Every generic parameter needs a literal value"))
             (let* ((key (table-radio-parameter-key key-word target))
                    (value
                      (cond
                        ((eq (first value-word) :string) (second value-word))
                        ((eq key :backend)
                         (let ((backend (string-downcase (second value-word))))
                           (unless (member backend
                                           '("org" "html" "latex" "texinfo")
                                           :test #'string=)
                             (table-radio-signal-error
                              target :invalid-parameters
                              "Only named Org, HTML, LaTeX, and Texinfo backends are supported"))
                           (intern (string-upcase backend) :keyword)))
                        (t (table-radio-parse-safe-atom
                            (second value-word) target)))))
               (when (and (not (eq key :backend))
                          (not (member key
                                       (table-radio-format-parameter-keys format)
                                       :test #'eq)))
                 (table-radio-signal-error
                  target :invalid-parameters
                  "Parameter is not supported by this table format"))
               (setf (getf parameters key)
                     (if (eq key :backend)
                         (intern (string-upcase (second value-word)) :keyword)
                         value))))
    parameters))

(defun table-radio-format-parameter-keys (format)
  (append '(:sep :hsep :hline :splice :skip
            :tstart :tend :lstart :lend :llstart :llend
            :hlstart :hlend :hllstart :hllend
            :fmt :hfmt :lfmt :llfmt :hlfmt :hllfmt
            :backend :raw)
          (case format
            (:latex '(:booktabs :environment))
            (:texinfo '(:columns))
            (otherwise nil))))

(defun table-radio-transform-format (name target)
  (let* ((name (string-downcase name))
         (name (if (table-radio-prefix-p name "orgtbl-to-")
                   (subseq name (length "orgtbl-to-"))
                   name)))
    (or (cdr (assoc name
                    '(("orgtbl" . :orgtbl)
                      ("generic" . :generic)
                      ("csv" . :csv)
                      ("tsv" . :tsv)
                      ("latex" . :latex)
                      ("texinfo" . :texinfo)
                      ("html" . :html))
                    :test #'string=))
        (table-radio-signal-error
         target :unsupported-format
         "Unsupported radio table transformation"))))

(defun table-radio-parse-send-line (line target)
  (let* ((trimmed (table-radio-trim-line line))
         (prefix "#+ORGTBL:")
         (value (and (table-radio-prefix-p trimmed prefix)
                     (string-left-trim '(#\Space #\Tab)
                                       (subseq trimmed (length prefix))))))
    (unless value
      (return-from table-radio-parse-send-line nil))
    (let ((words (table-radio-read-words value target)))
      (unless (and words
                   (eq (first (first words)) :atom)
                   (string-equal (second (first words)) "SEND"))
        (return-from table-radio-parse-send-line nil))
      (setf words (rest words))
      (unless (and (>= (length words) 2)
                   (every (lambda (word) (eq (first word) :atom))
                          (subseq words 0 2)))
        (table-radio-signal-error
         target :invalid-send-directive
         "SEND needs a table name and a transformation"))
      (let* ((name (second (first words)))
             (format (table-radio-transform-format
                      (second (second words)) target))
             (parameters (table-radio-safe-parameters
                          (nthcdr 2 words) format target)))
        (list :name name :format format :parameters parameters)))))

(defun table-radio-previous-send-directives (document table)
  (let* ((source (document-text document))
         (position (table-radio-line-start source (node-begin table)))
         (directives nil))
    (loop while (plusp position)
          for previous-end = (1- position)
          for previous-start = (table-radio-line-start source previous-end)
          for line-end = (table-radio-line-content-end source previous-start)
          for line = (subseq source previous-start line-end)
          do (if (table-radio-prefix-p (table-radio-trim-line line) "#+ORGTBL:")
                 (let ((directive (table-radio-parse-send-line line table)))
                   (if directive
                       (push directive directives)
                       (return))
                   (setf position previous-start))
                 (return)))
    (or (nreverse directives)
        (table-radio-signal-error
         table :missing-send-directive
         "No adjacent ORGTBL SEND directive was found"))))

(defun table-radio-table-rows (table)
  (unless (eq (node-property table :type) :org)
    (table-radio-signal-error
     table :unsupported-table-type
     "Only Org pipe tables can be sent"))
      (mapcar
   (lambda (row)
     (if (eq (node-property row :type) :rule)
         :hline
         (mapcar
          (lambda (cell)
            (let ((value (node-property cell :value)))
              (unless (stringp value)
                (table-radio-signal-error
                 table :invalid-table "Table cells must contain text"))
              value))
          (node-children row))))
   (node-children table)))

(defun table-radio-join (values separator)
  (with-output-to-string (stream)
    (loop for value in values
          for first-p = t then nil
          do (unless first-p (write-string separator stream))
             (write-string value stream))))

(defun table-radio-csv-cell (value)
  (table-import-export-write-rows
   (list (list value)) :csv :final-newline-p nil))

(defun table-radio-option (parameters key default)
  (if (member key parameters :test #'eq)
      (getf parameters key)
      default))

(defun table-radio-skip-rows (rows parameters target)
  (let ((skip (table-radio-option parameters :skip 0)))
    (unless (and (integerp skip) (not (minusp skip)))
      (table-radio-signal-error
       target :invalid-parameters ":skip must be a non-negative integer"))
    (nthcdr (min skip (length rows)) rows)))

(defun table-radio-delimited (rows separator csv-p parameters target)
  (let* ((rows (table-radio-skip-rows rows parameters target))
         (header-end (position :hline rows))
         (sep (if (member :sep parameters :test #'eq)
                  (or (getf parameters :sep) "")
                  separator)))
    (table-radio-join
     (loop for pair in
             (loop for row in rows for index from 0
                   collect (cons index row))
           append
           (let ((index (car pair))
                 (row (cdr pair)))
             (if (eq row :hline)
                 (let ((hline (getf parameters :hline)))
                   (if hline (list hline) nil))
                 (list
                  (table-radio-join
                   (mapcar (lambda (value)
                             (let ((format
                                     (table-radio-option
                                      parameters
                                      (if (and header-end (< index header-end))
                                          :hfmt :fmt)
                                      nil)))
                               (cond (format
                                      (table-radio-apply-format
                                       format value target))
                                     ((and csv-p
                                           (not (member :fmt parameters
                                                        :test #'eq)))
                                      (table-radio-csv-cell value))
                                     (t value))))
                           row)
                   (if (and header-end (< index header-end)
                            (member :hsep parameters :test #'eq))
                       (or (getf parameters :hsep) "")
                       sep))))))
     (string #\Newline))))

(defun table-radio-apply-format (template value target)
  (unless (stringp template)
    (table-radio-signal-error target :invalid-parameters
                              "Format templates must be literal strings"))
  (with-output-to-string (stream)
    (loop with position = 0
          while (< position (length template))
          do (if (and (char= (char template position) #\%)
                      (< (1+ position) (length template))
                      (char= (char template (1+ position)) #\s))
                 (progn
                   (write-string value stream)
                   (incf position 2))
                 (progn
                   (write-char (char template position) stream)
                   (incf position))))))

(defun table-radio-orgtbl (rows parameters target)
  (declare (ignore parameters))
  (let* ((data-rows (remove :hline rows))
         (column-count (reduce #'max data-rows :key #'length :initial-value 0))
         (widths (loop for column below column-count
                       collect
                       (loop for row in data-rows
                             maximize (length (or (nth column row) ""))
                               into width
                             finally (return (or width 0))))))
    (unless (plusp column-count)
      (table-radio-signal-error target :invalid-table
                                "Cannot send a table without cells"))
    (table-radio-join
     (mapcar
      (lambda (row)
        (if (eq row :hline)
            (concatenate
             'string "|"
             (table-radio-join
              (mapcar (lambda (width) (make-string (+ width 2) :initial-element #\-))
                      widths)
              "+")
             "|")
            (with-output-to-string (stream)
              (write-char #\| stream)
              (loop for width in widths
                    for column from 0
                    for value = (or (nth column row) "")
                    do (write-char #\Space stream)
                       (write-string value stream)
                       (loop repeat (- width (length value))
                             do (write-char #\Space stream))
                       (write-string " |" stream)))))
      rows)
     (string #\Newline))))

(defun table-radio-generic (rows parameters target)
  (let ((backend (getf parameters :backend)))
    (when backend
      (return-from table-radio-generic
        (case backend
          (:org (table-radio-orgtbl rows parameters target))
          (:html (table-radio-html rows parameters target))
          (:latex (table-radio-latex rows parameters target))
          (:texinfo (table-radio-texinfo rows parameters target))
          (otherwise
           (table-radio-signal-error
            target :invalid-parameters "Unsupported generic backend"))))))
  (let* ((rows (table-radio-skip-rows rows parameters target))
         (separator (table-radio-option parameters :sep ""))
         (header-end (position :hline rows))
         (table-lines
           (loop for row in rows
                 for index from 0
                 for last-p = (= index (1- (length rows)))
                 collect
                 (if (eq row :hline)
                     (table-radio-option parameters :hline nil)
                     (let* ((header-p (and header-end (< index header-end)))
                            (last-header-p
                              (and header-end (= index (1- header-end))))
                            (line-format
                              (cond
                                ((and last-header-p
                                      (getf parameters :hllfmt))
                                 (getf parameters :hllfmt))
                                ((and header-p (getf parameters :hlfmt))
                                 (getf parameters :hlfmt))
                                ((and last-p (getf parameters :llfmt))
                                 (getf parameters :llfmt))
                                ((getf parameters :lfmt) (getf parameters :lfmt))))
                            (line-start
                              (cond
                                ((and last-header-p
                                      (member :hllstart parameters :test #'eq))
                                 (getf parameters :hllstart))
                                ((and header-p
                                      (member :hlstart parameters :test #'eq))
                                 (getf parameters :hlstart))
                                ((and last-p
                                      (member :llstart parameters :test #'eq))
                                 (getf parameters :llstart))
                                (t (getf parameters :lstart))))
                            (line-end
                              (cond
                                ((and last-header-p
                                      (member :hllend parameters :test #'eq))
                                 (getf parameters :hllend))
                                ((and header-p
                                      (member :hlend parameters :test #'eq))
                                 (getf parameters :hlend))
                                ((and last-p
                                      (member :llend parameters :test #'eq))
                                 (getf parameters :llend))
                                (t (getf parameters :lend))))
                            (cell-format
                              (and (not line-format)
                                   (table-radio-option
                                    parameters (if header-p :hfmt :fmt) nil)))
                            (cell-separator
                              (if (and header-p
                                       (member :hsep parameters :test #'eq))
                                  (getf parameters :hsep)
                                  separator))
                            (contents
                              (table-radio-join
                               (mapcar
                                (lambda (value)
                                  (if (and cell-format (plusp (length value)))
                                      (table-radio-apply-format
                                       cell-format value target)
                                      value))
                                row)
                               (or cell-separator ""))))
                       (if line-format
                           (table-radio-apply-row-format line-format row target)
                           (concatenate 'string
                                        (or line-start "")
                                        contents
                                        (or line-end "")))))))
         (output (table-radio-join (remove nil table-lines)
                                   (string #\Newline)))
         (start (table-radio-option parameters :tstart nil))
         (end (table-radio-option parameters :tend nil)))
    (if (table-radio-option parameters :splice nil)
        output
        (concatenate 'string
                     (if start (concatenate 'string start (string #\Newline)) "")
                     output
                     (or end "")))))

(defun table-radio-apply-row-format (template values target)
  (unless (stringp template)
    (table-radio-signal-error target :invalid-parameters
                              "Row format templates must be literal strings"))
  (with-output-to-string (stream)
    (loop with position = 0
          with value-index = 0
          while (< position (length template))
          do (if (and (char= (char template position) #\%)
                      (< (1+ position) (length template))
                      (char= (char template (1+ position)) #\s))
                 (progn
                   (write-string (or (nth value-index values) "") stream)
                   (incf value-index)
                   (incf position 2))
                 (progn
                   (write-char (char template position) stream)
                   (incf position))))))

(defun table-radio-latex-escape (value)
  (let ((state (%make-export-latex-state :limits (make-limits))))
    (with-output-to-string (stream)
      (loop with start = 0
            for position = (position #\^ value :start start)
            do (write-string
                (export-latex-escape-text state (subseq value start position))
                stream)
            while position
            do (write-string "\\^{}" stream)
               (setf start (1+ position))))))

(defun table-radio-latex (rows parameters target)
  (let* ((data-rows (remove :hline rows))
         (columns (reduce #'max data-rows :key #'length :initial-value 0))
         (header-end (position :hline rows))
         (environment (table-radio-option parameters :environment "tabular"))
         (body nil))
    (unless (plusp columns)
      (table-radio-signal-error target :invalid-table
                                "Cannot send a table without cells"))
    (dolist (row rows)
      (cond
        ((eq row :hline) (push "\\hline" body))
        (t (push
            (concatenate
             'string
             (table-radio-join (mapcar #'table-radio-latex-escape row) " & ")
             "\\\\")
            body))))
    (when (and (getf parameters :booktabs) header-end)
      (let* ((rendered (nreverse body))
             (header-rows (subseq rendered 0 header-end))
             (body-rows (nthcdr (1+ header-end) rendered)))
        (setf body (nreverse
                    (append (list "\\toprule")
                            header-rows
                            (list "\\midrule")
                            body-rows
                            (list "\\bottomrule"))))))
    (format nil "\\begin{~A}{~A}~%~A~%\\end{~A}"
            environment (make-string columns :initial-element #\l)
            (table-radio-join (nreverse body) (string #\Newline))
            environment)))

(defun table-radio-texinfo-escape (value)
  (with-output-to-string (stream)
    (loop for character across value
          do (case character
               (#\@ (write-string "@@" stream))
               (#\{ (write-string "@{" stream))
               (#\} (write-string "@}" stream))
               (otherwise (write-char character stream))))))

(defun table-radio-texinfo (rows parameters target)
  (let* ((data-rows (remove :hline rows))
         (columns (reduce #'max data-rows :key #'length :initial-value 0))
         (widths (loop for column below columns
                       collect (loop for row in data-rows
                                     maximize (length (or (nth column row) ""))
                                       into width
                                     finally (return (max 1 (or width 0))))))
         (header-end (position :hline rows))
         (configured-columns (getf parameters :columns))
         (output-rows nil))
    (unless (plusp columns)
      (table-radio-signal-error target :invalid-table
                                "Cannot send a table without cells"))
    (dolist (row rows)
      (unless (eq row :hline)
        (let* ((index (position row rows :test #'eq))
               (header-p (and header-end (< index header-end)))
               (label (if (and header-p (= index (1- header-end)))
                          "@headitem"
                          "@item")))
          (push (format nil "~A ~A" label
                        (table-radio-texinfo-escape
                         (or (first row) "")))
                output-rows)
          (loop for value in (rest row)
                do (push (format nil "@tab ~A"
                                 (table-radio-texinfo-escape value))
                         output-rows)))))
    (let* ((body (table-radio-join (nreverse output-rows)
                                   (string #\Newline)))
           (column-spec
             (if (not configured-columns)
                 (table-radio-join
                  (mapcar (lambda (width)
                            (format nil "{~A}"
                                    (make-string width :initial-element #\a)))
                          widths)
                  " ")
                 (if (or (search "{" configured-columns)
                         (search "@columnfractions " configured-columns))
                     configured-columns
                     (concatenate 'string "@columnfractions "
                                  configured-columns)))))
      (format nil "@multitable ~A~%~A~%@end multitable"
              column-spec body))))

(defun table-radio-html-escape (value)
  (let ((state (%make-export-html-state :limits (make-limits))))
    (export-html-escape-to-output state :text value)
    (coerce (export-html-state-output state) 'string)))

(defun table-radio-html (rows parameters target)
  (declare (ignore parameters))
  (let* ((data-rows (remove :hline rows))
         (columns (reduce #'max data-rows :key #'length :initial-value 0))
         (header-end (position :hline rows))
         (headers (if header-end (subseq rows 0 header-end) nil))
         (body-rows (if header-end (nthcdr (1+ header-end) rows) rows)))
    (unless (plusp columns)
      (table-radio-signal-error target :invalid-table
                                "Cannot send a table without cells"))
    (with-output-to-string (stream)
      (write-string "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">" stream)
      (write-string (format nil "~%~%~%<colgroup>~%") stream)
      (loop for column below columns
            do (when (plusp column)
                 (write-string (format nil "~%~%") stream))
               (write-string "<col  class=\"org-left\" />" stream))
      (write-string (format nil "~%</colgroup>") stream)
      (when headers
        (write-string (format nil "~%<thead>~%<tr>") stream)
        (dolist (value (first headers))
          (write-string
           (format nil "~%<th scope=\"col\" class=\"org-left\">~A</th>"
                   (table-radio-html-escape value))
           stream))
        (write-string (format nil "~%</tr>~%</thead>") stream))
      (write-string (format nil "~%<tbody>") stream)
      (loop with first-row-p = nil
            for row in body-rows
            unless (eq row :hline)
              do (write-string (if first-row-p
                                   (format nil "~%~%")
                                   (format nil "~%"))
                               stream)
                 (setf first-row-p t)
                 (write-string "<tr>" stream)
                 (dolist (value row)
                   (write-string
                    (format nil "~%<td class=\"org-left\">~A</td>"
                            (table-radio-html-escape value))
                    stream))
                 (write-string (format nil "~%</tr>") stream))
      (write-string (format nil "~%</tbody>~%</table>") stream))))

(defun table-radio-render (table directive)
  (let* ((rows (table-radio-table-rows table))
         (format (getf directive :format))
         (parameters (getf directive :parameters))
         (target table))
    (case format
      (:orgtbl (table-radio-orgtbl rows parameters target))
      (:generic (table-radio-generic rows parameters target))
      (:csv (table-radio-delimited rows "," t parameters target))
      (:tsv (table-radio-delimited rows (string #\Tab) nil parameters target))
      (:latex (table-radio-latex rows parameters target))
      (:texinfo (table-radio-texinfo rows parameters target))
      (:html (table-radio-html rows parameters target))
      (otherwise
       (table-radio-signal-error target :unsupported-format
                                 "Unsupported radio table transformation")))))

(defun table-radio-marker-word-end (line start word)
  (when (table-radio-string-equal-at-p line start word)
    (let ((end (+ start (length word))))
      (when (or (= end (length line))
                (table-radio-horizontal-space-p (char line end)))
        end))))

(defun table-radio-marker-name-p (line marker name)
  (loop for start from 0 below (length line)
        for first-end = (table-radio-marker-word-end line start marker)
        when first-end
          do (let* ((second-start (table-radio-next-word line first-end))
                    (second-end
                      (table-radio-marker-word-end line second-start "RECEIVE"))
                    (third-start (and second-end
                                      (table-radio-next-word line second-end)))
                    (third-end
                      (and third-start
                           (table-radio-marker-word-end
                            line third-start "ORGTBL"))))
               (when third-end
                 (let* ((name-start (table-radio-next-word line third-end))
                        (name-end
                          (position-if #'table-radio-horizontal-space-p
                                       line :start name-start))
                        (actual (subseq line name-start
                                        (or name-end (length line)))))
                   (when (and (plusp (length actual))
                              (string-equal actual name))
                     (return t)))))
        finally (return nil)))

(defun table-radio-receive-regions (source name target)
  (let ((lines (table-radio-line-spans source))
        (regions nil))
    (loop with remaining = lines
          while remaining
          for begin-line = (pop remaining)
          for begin-text = (subseq source (first begin-line) (second begin-line))
          when (table-radio-marker-name-p begin-text "BEGIN" name)
            do (let ((end-index
                       (position-if
                        (lambda (line)
                          (table-radio-marker-name-p
                           (subseq source (first line) (second line)) "END" name))
                        remaining)))
                 (unless end-index
                   (table-radio-signal-error
                    target :missing-receive-end
                    "A RECEIVE marker has no matching END marker"))
                 (let* ((end-line (nth end-index remaining))
                        (body-start (third begin-line))
                        (body-end (first end-line)))
                   (push (cons body-start body-end) regions)
                   (setf remaining (nthcdr (1+ end-index) remaining)))))
    (or (nreverse regions)
        (table-radio-signal-error
         target :missing-receive-marker
         "No matching RECEIVE marker was found"))))

(defun table-send-radio (document target)
  "Transform a radio table and replace matching RECEIVE bodies.
Return the validated edit result, preserving all source outside those bodies."
  (let* ((table (edit-resolve-node document target '(:table)))
         (directives (table-radio-previous-send-directives document table))
         (source (document-text document))
         (replacements (make-hash-table :test #'equal)))
    (dolist (directive directives)
      (let* ((name (getf directive :name))
             (output (concatenate 'string
                                 (table-radio-render table directive)
                                 (string #\Newline)))
             (regions (table-radio-receive-regions source name table)))
        (dolist (region regions)
          (setf (gethash region replacements) output))))
    (let ((regions
            (sort (loop for region being the hash-keys of replacements
                        collect region)
                  #'< :key #'car)))
      (loop for first in regions
            for second in (rest regions)
            when (> (cdr first) (car second))
              do (table-radio-signal-error
                  table :overlapping-receive-regions
                  "RECEIVE replacement regions must not overlap"))
      (let* ((patches
              (mapcar (lambda (region)
                        (edit-make-patch (car region) (cdr region)
                                         (gethash region replacements) table))
                      regions))
             (prepared (edit-prepare-patches document patches :target table))
             (preview
               (edit-reparse-source
                document
                (edit-splice-source
                 (edit-prepared-patches-source prepared)
                 (edit-prepared-patches-patches prepared))
                table)))
        (setf (edit-prepared-patches-expected prepared)
              (edit-canonical-document preview))
        (edit-finish-prepared-patches prepared)))))

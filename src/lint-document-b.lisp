(in-package #:org-kit)

(defun %lint-b-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun %lint-b-lines (document function)
  (let ((text (document-text document))
        (start 0))
    (loop
      for newline = (position #\Newline text :start start)
      for end = (or newline (length text))
      for content-end = (if (and (> end start)
                                 (char= (char text (1- end)) #\Return))
                            (1- end)
                            end)
      do (funcall function start content-end (subseq text start content-end))
         (if newline
             (setf start (1+ newline))
             (return)))))

(defun %lint-b-node-at (document offset)
  (loop with best = nil
        for node in (%lint-nodes document)
        for begin = (node-begin node)
        for end = (node-end node)
        when (and begin end (<= begin offset) (< offset end)
                  (or (null best)
                      (<= (- end begin) (- (node-end best) (node-begin best))))) do
          (setf best node)
        finally (return best)))

(defun %lint-b-ancestor-type-p (node types)
  (loop for current = node then (node-parent current)
        while current
        thereis (member (node-type current) types :test #'eq)))

(defun %lint-b-prefix-p (text prefix &optional (start 0))
  (and (<= (+ start (length prefix)) (length text))
       (string-equal prefix text :start2 start :end2 (+ start (length prefix)))))

(defun %lint-b-leading-space-count (text)
  (or (position-if-not #'%lint-b-horizontal-space-p text) (length text)))

(defun %lint-b-drawer-line-p (line)
  (let* ((start (%lint-b-leading-space-count line))
         (end (loop for position downfrom (1- (length line)) to start
                    while (%lint-b-horizontal-space-p (char line position))
                    finally (return (1+ position)))))
    (and (<= (+ start 3) end)
         (char= (char line start) #\:)
         (char= (char line (1- end)) #\:)
         (loop for position from (1+ start) below (1- end)
               always (let ((character (char line position)))
                        (or (alphanumericp character)
                            (find character "-_"))))
         (subseq line start end))))

(defun %lint-b-drawer-close-p (document start end)
  (let ((found nil))
    (%lint-b-lines
     document
     (lambda (line-start line-end line)
       (declare (ignore line-end))
       (when (and (> line-start start)
                  (< line-start end)
                  (string-equal
                   (string-trim '(#\Space #\Tab #\Return) line)
                   ":END:"))
         (setf found t))))
    found))

(defun %lint-b-citation-prefix-end (text start)
  (let ((position (1+ start)))
    (when (and (<= (+ position 4) (length text))
               (string= "cite" text :start2 position :end2 (+ position 4)))
      (incf position 4)
      (when (and (< position (length text))
                 (char= (char text position) #\/))
        (incf position)
        (let ((style-start position))
          (loop while (and (< position (length text))
                           (let ((character (char text position)))
                             (or (alphanumericp character)
                                 (find character "/_-"))))
                do (incf position))
          (when (= position style-start)
            (return-from %lint-b-citation-prefix-end nil))))
      (when (and (< position (length text))
                 (char= (char text position) #\:))
        (incf position)
        (loop while (and (< position (length text))
                         (find (char text position)
                               '(#\Space #\Tab #\Newline #\Return)))
              do (incf position))
        position))))

(defun %lint-b-citation-prefix-p (text)
  (loop for start from 0 below (length text)
        thereis (and (char= (char text start) #\[)
                     (%lint-b-citation-prefix-end text start))))

(defun %lint-obsolete-include-markup (document context)
  (declare (ignore context))
  (loop for node in (%lint-nodes document :keyword)
        for value = (node-property node :value)
        when (and (string= (node-property node :key "") "INCLUDE")
                  (stringp value))
          append
            (let* ((length (length value))
                   (file-end
                     (if (and (plusp length) (char= (char value 0) #\"))
                         (let ((close (position #\" value :start 1)))
                           (and close (> close 1) (1+ close)))
                         (position-if #'%lint-b-horizontal-space-p value)))
                   (markup-start
                     (and file-end
                          (position-if-not #'%lint-b-horizontal-space-p
                                           value :start file-end)))
                   (markup-end
                     (and markup-start
                          (or (position-if #'%lint-b-horizontal-space-p
                                           value :start markup-start)
                              length)))
                   (markup (and markup-start (subseq value markup-start markup-end))))
              (when (and markup
                         (member markup
                                 '("ASCII" "BEAMER" "HTML" "LATEX" "MAN"
                                   "MARKDOWN" "MD" "ODT" "ORG" "TEXINFO")
                                 :test #'string-equal))
                (list (%lint-report
                       document "obsolete-include-markup"
                       (or (node-post-affiliated node) (node-begin node))
                       (format nil
                               "Obsolete markup \"~A\" in INCLUDE keyword.  Use \"export ~A\" instead"
                               markup markup)))))))

(defun %lint-invalid-keyword-syntax (document context)
  (declare (ignore context))
  (let ((reports nil))
    (%lint-b-lines
     document
     (lambda (start end line)
       (declare (ignore end))
       (let* ((prefix-start (%lint-b-leading-space-count line))
              (keyword-start (+ prefix-start 2)))
         (when (and (%lint-b-prefix-p line "#+" prefix-start)
                    (<= keyword-start (length line)))
           (let ((keyword-end
                   (loop for position from keyword-start below (length line)
                         while (and (not (%lint-b-horizontal-space-p
                                          (char line position)))
                                    (char/= (char line position) #\:))
                         finally (return position))))
             (when (or (= keyword-end (length line))
                       (%lint-b-horizontal-space-p (char line keyword-end)))
               (let ((name (subseq line keyword-start keyword-end)))
                 (unless (or (%lint-b-prefix-p name "BEGIN")
                             (%lint-b-prefix-p name "END"))
                   (push (%lint-report
                          document "invalid-keyword-syntax" start
                          (format nil "Possible missing colon in keyword \"~A\""
                                  name))
                         reports)))))))))
    (nreverse reports)))

(defun %lint-latex-dollar-ambiguous (document context)
  (declare (ignore context))
  (loop for fragment in (%lint-nodes document :latex-fragment)
        for begin = (node-begin fragment)
        for value = (node-property fragment :value)
        when (and begin (stringp value))
          append
            (loop for start = (position #\$ value)
                    then (position #\$ value :start (1+ start))
                  while start
                  when (and (< (+ start 2) (length value))
                            (digit-char-p (char value (+ start 2))))
                    collect
                      (%lint-report
                       document "LaTeX-$" (+ begin start 3)
                       "$ symbol potentially matching LaTeX fragment boundary.  Consider using \\dollar entity."))))

(defun %lint-b-weekday (year month day)
  (when (and (<= 1 month 12) (<= 1 day 31) (>= year 1))
    (let ((month-length
            (case month
              ((1 3 5 7 8 10 12) 31)
              ((4 6 9 11) 30)
              (2 (if (or (zerop (mod year 400))
                         (and (zerop (mod year 4))
                              (not (zerop (mod year 100)))))
                     29 28)))))
      (when (<= day month-length)
        (case (nth-value 6
               (decode-universal-time
                (encode-universal-time 0 0 12 day month year 0) 0))
          (0 "Mon")
          (1 "Tue")
          (2 "Wed")
          (3 "Thu")
          (4 "Fri")
          (5 "Sat")
          (6 "Sun"))))))

(defun %lint-b-unit-char (unit)
  (case unit
    (:hour #\h)
    (:day #\d)
    (:week #\w)
    (:month #\m)
    (:year #\y)))

(defun %lint-b-timestamp-adjustments (timestamp)
  (with-output-to-string (stream)
    (let ((mode (node-property timestamp :repeater-type))
          (amount (node-property timestamp :repeater-value))
          (unit (%lint-b-unit-char (node-property timestamp :repeater-unit))))
      (when (and mode amount unit)
        (format stream " ~A~D~C"
                (case mode (:cumulate "+") (:catch-up "++") (:restart ".+"))
                amount unit)))
    (let ((mode (node-property timestamp :warning-type))
          (amount (node-property timestamp :warning-value))
          (unit (%lint-b-unit-char (node-property timestamp :warning-unit))))
      (when (and mode amount unit)
        (format stream " ~A~D~C"
                (case mode (:first "--") (:all "-")) amount unit)))))

(defun %lint-b-timestamp-endpoint (timestamp suffix)
  (let* ((year (node-property timestamp (if suffix :year-end :year-start)))
         (month (node-property timestamp (if suffix :month-end :month-start)))
         (day (node-property timestamp (if suffix :day-end :day-start)))
         (hour (node-property timestamp (if suffix :hour-end :hour-start)))
         (minute (node-property timestamp (if suffix :minute-end :minute-start)))
         (weekday (and (integerp year) (integerp month) (integerp day)
                       (%lint-b-weekday year month day))))
    (when (and weekday (integerp year) (integerp month) (integerp day))
      (with-output-to-string (stream)
        (format stream "~4,'0D-~2,'0D-~2,'0D ~A" year month day weekday)
        (when (and (integerp hour) (integerp minute))
          (format stream " ~2,'0D:~2,'0D" hour minute))))))

(defun %lint-b-timestamp-canonical (timestamp)
  (let* ((type (node-property timestamp :type))
         (opening (case type ((:inactive :inactive-range) #\[)
                        ((:active :active-range) #\<)))
         (closing (case type ((:inactive :inactive-range) #\])
                        ((:active :active-range) #\>)))
         (range-p (member type '(:active-range :inactive-range) :test #'eq))
         (start (and opening (%lint-b-timestamp-endpoint timestamp nil)))
         (end (and opening (%lint-b-timestamp-endpoint timestamp t)))
         (hour (node-property timestamp :hour-start))
         (minute (node-property timestamp :minute-start))
         (end-hour (node-property timestamp :hour-end))
         (end-minute (node-property timestamp :minute-end))
         (same-date (and (= (node-property timestamp :year-start 0)
                            (node-property timestamp :year-end 0))
                         (= (node-property timestamp :month-start 0)
                            (node-property timestamp :month-end 0))
                         (= (node-property timestamp :day-start 0)
                            (node-property timestamp :day-end 0))))
         (time-range (and range-p same-date
                          (integerp hour) (integerp minute)
                          (integerp end-hour) (integerp end-minute))))
    (when (and opening start end)
      (with-output-to-string (stream)
        (write-char opening stream)
        (write-string start stream)
        (cond
          (time-range
           (format stream "-~2,'0D:~2,'0D" end-hour end-minute)
           (write-string (%lint-b-timestamp-adjustments timestamp) stream)
           (write-char closing stream))
          (range-p
           (write-string (%lint-b-timestamp-adjustments timestamp) stream)
           (write-char closing stream)
           (write-string "--" stream)
           (write-char opening stream)
           (write-string end stream)
           (write-string (%lint-b-timestamp-adjustments timestamp) stream)
           (write-char closing stream))
          (t
           (when (and (integerp hour) (integerp minute)
                      (integerp end-hour) (integerp end-minute)
                      (or (/= hour end-hour) (/= minute end-minute)))
             (format stream "-~2,'0D:~2,'0D" end-hour end-minute))
           (write-string (%lint-b-timestamp-adjustments timestamp) stream)
           (write-char closing stream)))))))

(defun %lint-timestamp-syntax (document context)
  (declare (ignore context))
  (loop for timestamp in (%lint-nodes document :timestamp)
        for expected = (%lint-b-timestamp-canonical timestamp)
        for raw = (node-text timestamp)
        for actual = (and raw
                          (string-trim '(#\Space #\Tab #\Return #\Newline) raw))
        when (and expected actual (not (string= expected actual)))
          collect
            (%lint-report
             document "timestamp-syntax" (node-begin timestamp)
             (format nil "Potentially malformed timestamp ~A.  Parsed as: ~A"
                     actual expected))))

(defun %lint-b-clock-canonical (clock)
  (let* ((timestamp (node-property clock :value))
         (timestamp-text (and timestamp (%lint-b-timestamp-canonical timestamp))))
    (when timestamp-text
      (let* ((line (node-text clock))
             (arrow (and line (search "=>" line)))
             (duration (and arrow
                            (string-trim (quote (#\Space #\Tab #\Return #\Newline))
                                         (subseq line (+ arrow 2)))))
             (formatted-duration
              (when (and duration (plusp (length duration)))
                (let ((colon (position #\: duration)))
                  (when colon
                    (let ((hours (subseq duration 0 colon))
                          (minutes (subseq duration (1+ colon))))
                      (format nil "~A:~A"
                              (if (< (length hours) 2) (concatenate (quote string) " " hours) hours)
                              (if (< (length minutes) 2)
                                  (concatenate (quote string) "0" minutes)
                                minutes))))))))
        (concatenate (quote string) "CLOCK: " timestamp-text
                     (if formatted-duration
                         (concatenate (quote string) " => " formatted-duration)
                       ""))))))

(defun %lint-clock-syntax (document context)
  (declare (ignore context))
  (loop for clock in (%lint-nodes document :clock)
        for expected = (%lint-b-clock-canonical clock)
        for raw = (node-text clock)
        for actual = (and raw
                          (string-trim '(#\Space #\Tab #\Return #\Newline) raw))
        when (and expected actual (not (string= expected actual)))
          collect
            (%lint-report
             document "clock-syntax" (node-begin clock)
             (format nil "Potentially malformed CLOCK: line~%           ~A~%Parsed as: ~A"
                     actual expected))))

(defun %lint-planning-inactive (document context)
  (declare (ignore context))
  (loop for planning in (%lint-nodes document :planning)
        for scheduled = (node-property planning :scheduled)
        for deadline = (node-property planning :deadline)
        when (and scheduled
                  (member (node-property scheduled :type)
                          '(:inactive :inactive-range) :test #'eq))
          collect
            (%lint-report
             document "planning-inactive" (node-begin planning)
             "Inactive timestamp in SCHEDULED will not appear in agenda.")
        else when (and deadline
                       (member (node-property deadline :type)
                               '(:inactive :inactive-range) :test #'eq))
          collect
            (%lint-report
             document "planning-inactive" (node-begin planning)
             "Inactive timestamp in DEADLINE will not appear in agenda.")))

(defun %lint-mismatched-planning-repeaters (document context)
  (declare (ignore context))
  (loop for planning in (%lint-nodes document :planning)
        for scheduled = (node-property planning :scheduled)
        for deadline = (node-property planning :deadline)
        for scheduled-mode = (and scheduled (node-property scheduled :repeater-type))
        for deadline-mode = (and deadline (node-property deadline :repeater-type))
        for scheduled-value = (and scheduled
                                   (node-property scheduled :repeater-value))
        for deadline-value = (and deadline
                                  (node-property deadline :repeater-value))
        for scheduled-unit = (and scheduled
                                  (node-property scheduled :repeater-unit))
        for deadline-unit = (and deadline
                                 (node-property deadline :repeater-unit))
        when (and scheduled deadline
                  (member scheduled-mode '(:cumulate :catch-up) :test #'eq)
                  (member deadline-mode '(:cumulate :catch-up) :test #'eq)
                  (typep scheduled-value '(integer 1 *))
                  (typep deadline-value '(integer 1 *))
                  (not (and (eq scheduled-mode deadline-mode)
                            (eq scheduled-unit deadline-unit)
                            (= scheduled-value deadline-value))))
          collect
            (%lint-report
             document "mismatched-planning-repeaters" (node-begin planning)
             "Different repeaters in SCHEDULED and DEADLINE timestamps.")))

(defun %lint-misplaced-planning-info (document context)
  (declare (ignore context))
  (let ((reports nil))
    (%lint-b-lines
     document
     (lambda (start end line)
       (declare (ignore end))
       (let* ((indent (%lint-b-leading-space-count line))
              (line-node (%lint-b-node-at document start)))
         (when (and (or (%lint-b-prefix-p line "CLOSED:" indent)
                        (%lint-b-prefix-p line "DEADLINE:" indent)
                        (%lint-b-prefix-p line "SCHEDULED:" indent))
                    (not (%lint-b-ancestor-type-p
                          line-node
                          '(:comment-block :example-block :export-block :planning
                            :src-block :verse-block))))
           (push (%lint-report document "misplaced-planning-info" start
                               "Misplaced planning info line")
                 reports)))))
    (nreverse reports)))

(defun %lint-incomplete-drawer (document context)
  (declare (ignore context))
  (let ((reports nil))
    (%lint-b-lines
     document
     (lambda (start end line)
       (declare (ignore end))
       (let* ((drawer-line (%lint-b-drawer-line-p line))
              (node (%lint-b-node-at document start))
              (inside-drawer (%lint-b-ancestor-type-p node '(:drawer)))
              (incomplete-root-drawer
                (and node (eq (node-type node) :drawer) (= (node-begin node) start)
                     (not (%lint-b-drawer-close-p document start
                                                  (node-end node)))))
              (protected (%lint-b-ancestor-type-p
                          node '(:property-drawer :comment-block :example-block
                                 :export-block :src-block :verse-block))))
         (when (and drawer-line
                    (not protected)
                    (if inside-drawer
                        (not (string-equal drawer-line ":END:"))
                        (not (%lint-b-ancestor-type-p node '(:drawer :property-drawer))))
           (push
            (%lint-report
             document "incomplete-drawer" start
             (if incomplete-root-drawer
                 (format nil "Possible incomplete drawer ~S" drawer-line)
                 (if inside-drawer
                 (format nil "Possible misleading drawer entry ~S" drawer-line)
                 (format nil "Possible incomplete drawer ~S" drawer-line))))
            reports))))))
    (nreverse reports)))

(defun %lint-indented-diary-sexp (document context)
  (declare (ignore context))
  (let ((reports nil))
    (%lint-b-lines
     document
     (lambda (start end line)
       (declare (ignore end))
       (let* ((indent (%lint-b-leading-space-count line))
              (node (%lint-b-node-at document start)))
         (when (and (plusp indent)
                    (%lint-b-prefix-p line "%%(" indent)
                    (not (%lint-b-ancestor-type-p
                          node '(:comment-block :diary-sexp :example-block
                                 :export-block :src-block :verse-block))))
           (push (%lint-report document "indented-diary-sexp" start
                               "Possible indented diary-sexp")
                 reports)))))
    (nreverse reports)))

(defun %lint-quote-section (document context)
  (declare (ignore context))
  (loop for node in (%lint-nodes document)
        for title = (node-property node :raw-value "")
        when (and (member (node-type node) '(:headline :inlinetask) :test #'eq)
                  (stringp title)
                  (or (and (<= 6 (length title))
                           (string= "QUOTE " title :end2 6))
                      (and (<= 14 (length title))
                           (string= "COMMENT QUOTE " title :end2 14))))
          collect (%lint-report document "quote-section" (node-begin node)
                                "Deprecated QUOTE section")))

(defun %lint-file-application (document context)
  (declare (ignore context))
  (loop for link in (%lint-nodes document :link)
        for type = (node-property link :type)
        when (and (stringp type) (%lint-b-prefix-p type "file+"))
          collect
            (%lint-report document "file-application" (node-begin link)
                          (format nil "Deprecated \"file+~A\" link type"
                                  (subseq type (length "file+"))))))

(defun %lint-percent-encoding-link-escape (document context)
  (declare (ignore context))
  (loop for link in (%lint-nodes document :link)
        for path = (node-property link :path)
        when (and (eq (node-property link :format) :bracket)
                  (stringp path)
                  (loop for start = (position #\% path)
                          then (position #\% path :start (+ start 3))
                        while start
                        for end = (min (length path) (+ start 3))
                        for escape = (subseq path start end)
                        thereis (or (< (length escape) 3)
                                    (not (member escape
                                                '("%25" "%5B" "%5D" "%20")
                                                :test #'string=)))))
          collect
            (%lint-report document "percent-encoding-link-escape"
                          (node-begin link)
                          "Link escaped with obsolete percent-encoding syntax")))

(defun %lint-spurious-colons (document context)
  (declare (ignore context))
  (loop for headline in (%lint-nodes document :headline)
        for tags = (node-property headline :tags)
        for line = (let* ((start (node-begin headline))
                          (text (document-text document))
                          (end (or (position #\Newline text :start start)
                                   (length text))))
                     (subseq text start end))
        for tag-separator = (loop for position from 1 below (length line)
                                  when (and (char= (char line position) #\:)
                                            (member (char line (1- position))
                                                    '(#\Space #\Tab)))
                                    return position)
        for spurious-suffix = (and tag-separator
                                   (search "::" line :start2 tag-separator)
                                   (= (+ (search "::" line :start2 tag-separator) 2)
                                      (length line)))
        when (or (and (listp tags) (member "" tags :test #'string=))
                 spurious-suffix)
          collect (%lint-report document "spurious-colons" (node-begin headline)
                                "Tags contain a spurious colon")))

(defun %lint-missing-print-bibliography (document context)
  (declare (ignore context))
  (when (and (%lint-nodes document :citation)
             (not (loop for keyword in (%lint-nodes document :keyword)
                        thereis (string= (node-property keyword :key "")
                                         "PRINT_BIBLIOGRAPHY"))))
    (list (%lint-report document "missing-print-bibliography"
                        (length (document-text document))
                        "Possibly missing \"PRINT_BIBLIOGRAPHY\" keyword"))))

(defun %lint-incomplete-citation (document context)
  (declare (ignore context))
  (loop for text-node in (%lint-nodes document :plain-text)
        for text = (node-text text-node)
        for paragraph = (loop for node = (node-parent text-node) then (node-parent node)
                              while node
                              when (eq (node-type node) :paragraph)
                                return node)
        when (and (stringp text) (%lint-b-citation-prefix-p text))
          collect
            (%lint-report
             document "incomplete-citation"
             (or (and paragraph (node-contents-begin paragraph))
                 (node-begin text-node))
             "Possibly incomplete citation markup")))

(defun %lint-b-bullet-number (bullet)
  (when (stringp bullet)
    (let ((letter (position-if #'alpha-char-p bullet)))
      (cond
        (letter (1+ (- (char-code (char-upcase (char bullet letter)))
                      (char-code #\A))))
        (t (let ((digit (position-if #'digit-char-p bullet)))
             (when digit
               (parse-integer bullet :start digit :junk-allowed t))))))))

(defun %lint-item-number (document context)
  (declare (ignore context))
  (let ((reports nil))
    (dolist (list-node (%lint-nodes document :plain-list))
      (loop with expected = 1
            for item in (node-children list-node)
            for bullet = (node-property item :bullet)
            for bullet-number = (%lint-b-bullet-number bullet)
            for counter = (node-property item :counter)
            when (and (integerp bullet-number)
                      (not counter)
                      (/= bullet-number expected))
              do (push
                  (%lint-report
                   document "item-number" (node-begin item)
                   (format nil
                           "Bullet counter \"~A\" is not the same with item position ~D.  Consider adding manual [@~D] counter."
                           bullet expected bullet-number))
                  reports)
            do (setf expected
                     (1+ (if (integerp counter)
                             counter
                             (or bullet-number expected))))))
    (nreverse reports)))

(defun %lint-b-priority-string (priority)
  (if (<= 65 priority 90)
      (string (code-char priority))
      (format nil "~D" priority)))

(defun %lint-priority (document context)
  (declare (ignore context))
  (let ((reports nil))
    (dolist (headline (%lint-nodes document :headline))
      (let* ((priority (node-property headline :priority))
             (raw-value (node-property headline :raw-value "")))
        (cond
          ((and (integerp priority)
                (or (<= 0 priority 64) (<= 65 priority 90))
                (not (<= 65 priority 67)))
           (push
            (%lint-report
             document "priority" (node-begin headline)
             (format nil "Out-of-bounds priority '~A'"
                     (%lint-b-priority-string priority)))
            reports))
          ((stringp raw-value)
           (let ((start (and (<= 2 (length raw-value))
                             (string= "[#" raw-value :end2 2)
                             0)))
             (when start
               (let* ((body-start (+ start 2))
                      (end (loop for position from body-start below (length raw-value)
                                 until (find (char raw-value position) "[]")
                                 finally (return position)))
                      (closed (and (< end (length raw-value))
                                   (char= (char raw-value end) #\])))
                      (body (subseq raw-value body-start end)))
                 (push
                  (%lint-report
                   document "priority" (node-begin headline)
                   (if closed
                       (format nil "Invalid priority '~A'" body)
                       (format nil "Malformed priority '~A'"
                               (subseq raw-value start))))
                  reports))))))))
    (nreverse reports)))

(defun %lint-b-attribute-value (text name)
  (loop for start = (search name text :test #'char=)
          then (search name text :start2 (1+ start) :test #'char=)
        while start
        for after = (+ start (length name))
        when (and (< after (length text))
                  (%lint-b-horizontal-space-p (char text after)))
          do (let* ((value-start
                      (position-if-not #'%lint-b-horizontal-space-p
                                       text :start after))
                    (value-end
                      (and value-start
                           (or (position-if #'%lint-b-horizontal-space-p
                                            text :start value-start)
                               (length text)))))
               (when value-start
                 (return (subseq text value-start value-end))))))

(defun %lint-invalid-image-alignment (document context)
  (declare (ignore context))
  (loop for paragraph in (%lint-nodes document :paragraph)
        for attributes = (node-property paragraph :ATTR_ORG)
        when (stringp attributes)
          append
            (loop for (key allowed message-key) in
                    '((":align" ("left" "center" "right") "align")
                      (":center" ("t") "center"))
                  for value = (%lint-b-attribute-value attributes key)
                  when (and value (not (member value allowed :test #'string=)))
                    collect
                      (%lint-report
                       document "invalid-image-alignment"
                       (or (node-begin paragraph) 0)
                       (format nil
                               "\"~A\" not a supported value for #+ATTR_ORG keyword attribute \":~A\"."
                               value message-key)))))

(defun %lint-invalid-block (document context)
  (declare (ignore context))
  (let ((reports nil)
        (allowed-types (quote (:center-block :comment-block :dynamic-block
                              :example-block :export-block :quote-block
                              :special-block :src-block :verse-block))))
    (%lint-b-lines
     document
     (lambda (start end line)
       (declare (ignore end))
       (let* ((indent (%lint-b-leading-space-count line))
              (marker-start (+ indent 2)))
         (when (and (%lint-b-prefix-p line "#+" indent)
                    (< marker-start (length line)))
           (let* ((kind (cond ((%lint-b-prefix-p line "BEGIN" marker-start) "BEGIN")
                              ((%lint-b-prefix-p line "END" marker-start) "END")))
                  (after-kind (and kind (+ marker-start (length kind))))
                  (suffix-end
                    (and after-kind
                         (cond
                           ((and (< after-kind (length line))
                                 (char= (char line after-kind) #\:))
                            (1+ after-kind))
                           ((and (< after-kind (length line))
                                 (char= (char line after-kind) #\_))
                            (or (position-if (function %lint-b-horizontal-space-p)
                                             line :start (1+ after-kind))
                                (length line)))
                           ((or (= after-kind (length line))
                                (%lint-b-horizontal-space-p
                                 (char line after-kind)))
                            after-kind))))
                  (node (and suffix-end (%lint-b-node-at document start)))
                  (trimmed (string-trim (quote (#\Space #\Tab)) line)))
             (when (and suffix-end
                        (if (string= kind "END")
                            (let ((trailing-start
                                    (position-if-not (function %lint-b-horizontal-space-p)
                                                     line :start suffix-end)))
                              (and trailing-start
                                   (not (= trailing-start (length line)))))
                            (not (and node
                                      (%lint-b-ancestor-type-p node allowed-types)))))
               (push
                (%lint-report
                 document "invalid-block" start
                 (if (string= kind "END")
                     (format nil "Invalid block closing line \"~A\"" trimmed)
                     (format nil "Possible incomplete block \"~A\"" trimmed)))
                reports)))))))
    (nreverse reports)))


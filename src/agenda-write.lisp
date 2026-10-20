;;;; src/agenda-write.lisp
(in-package #:org-kit)

(defun agenda-write-signal-invalid (context message)
  (agenda-signal-error :invalid-option context message))

(defun agenda-write-view-result (view)
  (cond
    ((agenda-custom-view-p view) (agenda-custom-view-result view))
    ((agenda-result-p view) view)
    ((and (listp view)
          (integerp (agenda-proper-list-length view))
          (every #'agenda-match-entry-p view)) view)
    (t
     (agenda-signal-error :unsupported-entry
                          view
                          "Agenda output requires an agenda result or match entries"))))

(defun agenda-write-result-entries (view)
  (let* ((result (agenda-write-view-result view))
         (entries
          (if (agenda-result-p result)
              (agenda-result-entries result)
              result)))
    (unless
        (and (integerp (agenda-proper-list-length entries))
             (every
              (lambda (entry)
                (or (agenda-entry-p entry) (agenda-match-entry-p entry)))
              entries))
      (agenda-signal-error :unsupported-entry
                           entries
                           "Agenda output contains an unsupported entry"))
    entries))

(defun agenda-write-entry-document (entry)
  (if (agenda-match-entry-p entry)
      (agenda-match-entry-document entry)
      (let ((source (agenda-entry-source-node entry)))
        (and source (node-document source)))))

(defun agenda-write-result-documents (entries)
  (let ((documents nil))
    (dolist (entry entries)
      (let ((document (agenda-write-entry-document entry)))
        (when
            (and (document-p document)
                 (not (member document documents :test #'eq)))
          (push document documents))))
    (nreverse documents)))

(defun agenda-write-limits (documents)
  (let ((limits (mapcar #'document-limits documents)))
    (make-limits :max-input-length
                 (minimum-limit-value limits :max-input-length)
                 :max-heading-depth
                 (minimum-limit-value limits :max-heading-depth)
                 :max-element-depth
                 (minimum-limit-value limits :max-element-depth)
                 :max-object-depth
                 (minimum-limit-value limits :max-object-depth)
                 :max-node-count
                 (minimum-limit-value limits :max-node-count)
                 :max-output-length
                 (minimum-limit-value limits :max-output-length)
                 :max-output-octets
                 (minimum-limit-value limits :max-output-octets)
                 :max-agenda-entries
                 (minimum-limit-value limits :max-agenda-entries))))

(defun agenda-write-check-entry-limit (entries documents)
  (check-limit (agenda-write-limits documents)
               :max-agenda-entries
               (length entries)))

(defun agenda-write-check-output (output documents)
  (let ((limits (agenda-write-limits documents)))
    (check-limit limits :max-output-length (length output))
    (check-limit limits
                 :max-output-octets
                 (cl-codec-kit:string-size-in-octets output :encoding :utf-8)))
  output)

(defun agenda-write-date-ordinal (value)
  (when (org-time-value-p value)
    (cl-date-kit:local-date-to-epoch-day
     (cl-date-kit:make-local-date (org-time-value-year value)
                                  (org-time-value-month value)
                                  (org-time-value-day value)))))

(defun agenda-write-date-week-index (date)
  (position (cl-date-kit:day-of-week date)
            '(:monday :tuesday :wednesday :thursday :friday :saturday :sunday)
            :test #'eq))

(defun agenda-write-iso-week-number (date)
  (let* ((weekday (agenda-write-date-week-index date))
         (thursday (cl-date-kit:local-date-plus-days date (- 3 weekday)))
         (iso-year (cl-date-kit:local-date-year thursday))
         (jan-fourth (cl-date-kit:make-local-date iso-year 1 4))
         (first-monday
           (cl-date-kit:local-date-plus-days
            jan-fourth
            (- (agenda-write-date-week-index jan-fourth))))
         (current-monday
           (cl-date-kit:local-date-plus-days date (- weekday))))
    (1+ (floor (- (cl-date-kit:local-date-to-epoch-day current-monday)
                  (cl-date-kit:local-date-to-epoch-day first-monday))
               7))))

(defun agenda-write-date-heading (value)
  (unless (and (org-time-value-p value)
               (eq (org-time-value-kind value) :date))
    (agenda-write-signal-invalid value
                                 "Agenda output dates must be date-only values"))
  (let* ((date (cl-date-kit:make-local-date (org-time-value-year value)
                                             (org-time-value-month value)
                                             (org-time-value-day value)))
         (weekday (cl-date-kit:day-of-week date))
         (weekday-name
           (cdr (assoc weekday
                       '((:monday . "Monday")
                         (:tuesday . "Tuesday")
                         (:wednesday . "Wednesday")
                         (:thursday . "Thursday")
                         (:friday . "Friday")
                         (:saturday . "Saturday")
                         (:sunday . "Sunday")))))
         (month-name
           (aref #("January" "February" "March" "April" "May" "June"
                   "July" "August" "September" "October" "November" "December")
                 (1- (org-time-value-month value))))
         (week-suffix
           (if (eq weekday :monday)
               (format nil " W~2,'0D" (agenda-write-iso-week-number date))
               "")))
    (format nil "~10A ~2D ~A ~4D~A"
            weekday-name
            (org-time-value-day value)
            month-name
            (org-time-value-year value)
            week-suffix)))

(defun agenda-write-category-field (category)
  (let* ((category-text
           (cond
             ((null category) "")
             ((stringp category) category)
             ((symbolp category) (symbol-name category))
             (t
              (agenda-write-signal-invalid
               category
               "Agenda categories must be strings or symbols"))))
         (field (if (equal category-text "")
                    ""
                    (concatenate 'string category-text ":"))))
    (concatenate 'string
                 field
                 (make-string (max 0 (- 12 (length field)))
                              :initial-element #\Space))))

(defun agenda-write-entry-extra (entry)
  (let* ((row-date (agenda-write-date-ordinal (agenda-entry-date entry)))
         (timestamp (agenda-entry-timestamp entry))
         (start (and (org-timestamp-view-p timestamp)
                     (org-timestamp-view-start timestamp)))
         (end (and (org-timestamp-view-p timestamp)
                   (org-timestamp-view-end timestamp)))
         (deadline-date (agenda-write-date-ordinal (or end start)))
         (scheduled-date (agenda-write-date-ordinal start)))
    (case (agenda-entry-kind entry)
      (:deadline
       (case (agenda-entry-warning-state entry)
         (:prewarning
          (format nil "In ~3D d.: " (- deadline-date row-date)))
         (:overdue
          (format nil "~2D d. ago: " (- row-date deadline-date)))
         (otherwise "Deadline:  ")))
      (:scheduled
       (if (eq (agenda-entry-scheduled-delay-state entry) :delayed)
           (format nil "Sched.~2Dx: " (- row-date scheduled-date))
           "Scheduled: "))
      (otherwise nil))))

(defun agenda-write-entry-visible-time-p (entry)
  (not (or (and (eq (agenda-entry-kind entry) :deadline)
                (member (agenda-entry-warning-state entry)
                        '(:prewarning :overdue)))
           (and (eq (agenda-entry-kind entry) :scheduled)
                (eq (agenda-entry-scheduled-delay-state entry) :delayed)
                (not (agenda-entry-habit-p entry))))))

(defun agenda-write-time-text (timestamp &optional (visible-p t))
  (when timestamp
    (unless (org-timestamp-view-p timestamp)
      (agenda-write-signal-invalid timestamp
                                   "Agenda timestamps must be normalized views"))
    (when visible-p
      (let* ((start (org-timestamp-view-start timestamp))
             (end (org-timestamp-view-end timestamp))
             (hour (and start (org-time-value-hour start)))
             (end-hour (and end (org-time-value-hour end))))
        (when hour
          (let ((start-text
                  (format nil "~D:~2,'0D"
                          hour
                          (org-time-value-minute start))))
            (with-output-to-string (stream)
              (write-string
               (make-string (max 0 (- 5 (length start-text)))
                            :initial-element #\Space)
               stream)
              (write-string start-text stream)
              (if end-hour
                  (progn
                    (write-char #\- stream)
                    (format stream
                            "~D:~2,'0D"
                            end-hour
                            (org-time-value-minute end)))
                  (write-string "......" stream)))))))))

(defun agenda-write-tag-suffix (tags)
  (if tags
      (with-output-to-string (stream)
        (write-string " :" stream)
        (loop for tag in tags
              for firstp = t then nil
              do (unless firstp
                   (write-char #\: stream)) (write-string tag stream))
        (write-char #\: stream))
      ""))

(defun agenda-write-entry-line (todo headline tags time category view-type extra)
  (let* ((body
           (string-trim '(#\Space #\Tab #\Return #\Newline)
                        (with-output-to-string (stream)
                          (when todo
                            (write-string todo stream)
                            (when headline
                              (write-char #\Space stream)))
                          (when headline
                            (write-string headline stream)))))
         (category-field (agenda-write-category-field category)))
    (with-output-to-string (stream)
      (write-string "  " stream)
      (write-string category-field stream)
      (when (eq view-type :agenda)
        (when time
          (write-string time stream)
          (write-string
           (make-string (max 0 (- 12 (length time)))
                        :initial-element #\Space)
           stream))
        (when extra
          (write-string extra stream)
          (write-char #\Space stream)))
      (write-string body stream)
      (when tags
        (write-string
         (make-string (max 1 (- 50 (length body)))
                      :initial-element #\Space)
         stream)
        (write-string (agenda-write-tag-suffix tags) stream))
      (terpri stream))))

(defun agenda-write-match-entry-line (entry view-type)
  (multiple-value-bind (document heading) (agenda-write-entry-heading entry)
    (agenda-write-entry-line
     (node-property heading :todo-keyword)
     (node-property heading :raw-value)
     (effective-heading-tags document heading)
     nil
     (agenda-heading-category document heading)
     view-type
     nil)))

(defun agenda-write-agenda-text (entries &optional (view-type :agenda))
  (with-output-to-string (stream)
    (let ((previous-date nil)
          (previous-date-p nil))
      (dolist (entry entries)
        (if (agenda-match-entry-p entry)
            (write-string (agenda-write-match-entry-line entry view-type) stream)
            (let* ((date (agenda-entry-date entry))
                   (timestamp (agenda-entry-timestamp entry)))
              (when (and (eq view-type :agenda) date)
                (let ((date-heading (agenda-write-date-heading date))
                      (date-text (org-time-value-iso8601 date)))
                  (unless (and previous-date-p
                               (string= date-text previous-date))
                    (write-string date-heading stream)
                    (terpri stream))
                  (setf previous-date date-text
                        previous-date-p t)))
              (write-string
               (agenda-write-entry-line
                (agenda-entry-todo-state entry)
                (agenda-entry-headline entry)
                (agenda-entry-effective-tags entry)
                (and (eq view-type :agenda)
                     (agenda-write-time-text
                      timestamp
                      (agenda-write-entry-visible-time-p entry)))
                (agenda-entry-category entry)
                view-type
                (and (eq view-type :agenda)
                     (agenda-write-entry-extra entry)))
               stream)))))))

(defun agenda-format-text (view)
  "Format stable agenda text with date headings and available time, category, and tags."
  (let* ((result (agenda-write-view-result view))
         (entries (agenda-write-result-entries result))
         (documents (agenda-write-result-documents entries))
         (view-type (cond
                      ((agenda-custom-view-p view)
                       (agenda-custom-view-type view))
                      ((agenda-result-p result) :agenda)
                      (t :search))))
    (agenda-write-check-entry-limit entries documents)
    (agenda-write-check-output
     (agenda-write-agenda-text entries view-type)
     documents)))

(defun agenda-write-entry-heading (entry)
  (if (agenda-match-entry-p entry)
      (let ((document (agenda-match-entry-document entry))
            (node (agenda-match-entry-node entry)))
        (unless
            (and (document-p document)
                 (node-p node)
                 (eq (node-document node) document)
                 (eq (node-type node) :headline))
          (agenda-signal-error :unsupported-entry
                               entry
                               "Agenda match entries must identify a source headline"))
        (values document node))
      (let ((source (agenda-entry-source-node entry)))
        (unless (node-p source)
          (agenda-signal-error :unsupported-entry
                               entry
                               "Agenda entries must identify a source headline"))
        (let ((heading
               (loop for node = source then (node-parent node)
                     while node
                     when (eq (node-type node) :headline)
                       return node)))
          (unless heading
            (agenda-signal-error :unsupported-entry
                                 entry
                                 "Agenda entries must identify a source headline"))
          (let ((document (node-document heading)))
            (unless (document-p document)
              (agenda-signal-error :unsupported-entry
                                   entry
                                   "Agenda entries must identify a source document"))
            (values document heading))))))

(defun agenda-write-selected-headings (entries)
  (let ((selected nil))
    (dolist (entry entries)
      (multiple-value-bind (document heading) (agenda-write-entry-heading entry)
        (unless
            (find-if
             (lambda (selection)
               (and (eq document (car selection)) (eq heading (cdr selection))))
             selected)
          (push (cons document heading) selected))))
    (nreverse selected)))

(defun agenda-write-subtree-headings (heading)
  (let ((headings nil))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :headline)
         (push node headings)))
     heading)
    (nreverse headings)))

(defun agenda-write-subtree-fragment (document heading)
  (let* ((source (document-text document))
         (begin (node-begin heading))
         (headings (agenda-write-subtree-headings heading))
         (child (second headings))
         (end (if child (node-begin child) (node-end heading)))
         (base-stars (nth-value 2 (edit-subtree-marker document heading))))
    (unless
        (and (integerp begin)
             (integerp end)
             (<= 0 begin end (length source))
             (eq (first headings) heading)
             (plusp base-stars))
      (agenda-signal-error :unsupported-entry
                           heading
                           "Agenda source subtrees must have valid headline ranges"))
    (values
     (subseq source begin end)
     (let ((fragment
             (with-output-to-string (stream)
               (let ((cursor begin))
                 (multiple-value-bind (marker-start marker-end stars)
                     (edit-subtree-marker document heading)
                   (unless
                       (and (<= begin marker-start marker-end end)
                            (= stars base-stars))
                     (agenda-signal-error :unsupported-entry
                                          heading
                                          "Agenda source subtree heading levels are invalid"))
                   (write-string (subseq source cursor marker-start) stream)
                   (write-string (make-string 1 :initial-element #\*) stream)
                   (setf cursor marker-end))
                 (write-string (subseq source cursor end) stream)))))
       (when (and (> base-stars 1)
                  (node-property heading :tags))
         (let* ((tags (node-property heading :tags))
                (tag-string (format nil ":~{~A~^:~}:" tags))
                (marker-start
                  (- (nth-value 0 (edit-subtree-marker document heading))
                     begin))
                (line-start
                  (let ((newline (position #\Newline fragment
                                           :end marker-start
                                           :from-end t)))
                    (if newline (1+ newline) 0)))
                (line-end
                  (or (position #\Newline fragment :start marker-start)
                      (length fragment)))
                (line (subseq fragment line-start line-end))
                (tags-start (search tag-string line :from-end t)))
           (when tags-start
             (let* ((blank-start
                      (1+ (or (position-if-not
                               (lambda (character)
                                 (find character '(#\Space #\Tab)))
                               line
                               :end tags-start
                               :from-end t)
                              -1)))
                    (prefix (subseq line 0 blank-start))
                    (prefix-width
                      (loop with width = 0
                            for character across prefix
                            do (incf width
                                     (if (char= character #\Tab)
                                         (- 8 (mod width 8))
                                         (table-character-width character)))
                            finally (return width)))
                    (tag-column
                      (max (- 77 (table-string-width tag-string))
                           (1+ prefix-width)))
                    (aligned-line
                      (concatenate
                       'string
                       prefix
                       (make-string (- tag-column prefix-width)
                                    :initial-element #\Space)
                       tag-string
                       (subseq line (+ tags-start (length tag-string))))))
               (setf fragment
                     (concatenate 'string
                                  (subseq fragment 0 line-start)
                                  aligned-line
                                  (subseq fragment line-end)))))))
       fragment))))

(defun agenda-write-line-ending (documents)
  (if (and documents (eq (document-newline (first documents)) :crlf))
      (format nil "~C~C" #\Return #\Newline)
      (string #\Newline)))

(defun agenda-write-join-fragments (fragments newline)
  (with-output-to-string (stream)
    (loop for fragment in fragments
          for firstp = t then nil
          do (unless firstp
               (write-string newline stream))
             (write-string
              (string-right-trim '(#\Return #\Newline) fragment)
              stream))
    (when fragments
      (write-string newline stream))))

(defun agenda-to-org (view)
  "Return a parsed Org document containing selected headline text."
  (let* ((entries (agenda-write-result-entries view))
         (documents (agenda-write-result-documents entries)))
    (agenda-write-check-entry-limit entries documents)
    (let* ((selected (agenda-write-selected-headings entries))
           (selected-documents
            (remove-duplicates (mapcar #'car selected) :test #'eq))
           (fragments nil)
           (source-fragments nil))
      (dolist (selection selected)
        (multiple-value-bind (source-fragment fragment)
            (agenda-write-subtree-fragment (car selection) (cdr selection))
          (unless (member source-fragment source-fragments :test #'string=)
            (push source-fragment source-fragments)
            (push fragment fragments))))
      (let* ((output
              (agenda-write-join-fragments (nreverse fragments)
                                           (agenda-write-line-ending
                                            selected-documents)))
             (limits (agenda-write-limits documents)))
        (agenda-write-check-output output documents)
        (parse-org output :limits limits)))))

(defun agenda-write-output (view format)
  (case format
    (:text (agenda-format-text view))
    (:org (write-org (agenda-to-org view) :output-type :string))
    (t
     (agenda-write-signal-invalid format
                                  "Agenda output format must be :text or :org"))))

(defun agenda-write-into (document view start end &key (format :text))
  "Replace the caller's explicit source range with formatted agenda output."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (let* ((source (document-text document))
         (replacement (agenda-write-output view format))
         (patches
           (edit-validate-patches
            source
            (list (edit-make-patch start end replacement :agenda))
            :agenda))
         (updated-source (edit-splice-source source patches))
         (expected
           (edit-canonical-document
            (edit-reparse-source document updated-source :agenda))))
    (edit-apply-patches-result document patches
                               :target :agenda
                               :expected expected)))

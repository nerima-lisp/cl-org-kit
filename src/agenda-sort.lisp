(in-package #:org-kit)

(defparameter +agenda-sort-view-strategies+
  '(:time-up :time-down
    :timestamp-up :timestamp-down
    :scheduled-up :scheduled-down
    :deadline-up :deadline-down
    :ts-up :ts-down
    :tsia-up :tsia-down
    :category-keep :category-up :category-down
    :tag-up :tag-down
    :priority-up :priority-down
    :stats-up :stats-down
    :urgency-up :urgency-down
    :todo-state-up :todo-state-down
    :effort-up :effort-down
    :habit-up :habit-down
    :alpha-up :alpha-down
    :user-defined-up :user-defined-down))

(defun agenda-sort-view-invalid-option (value message)
  (agenda-signal-error :invalid-option value message))

(defun agenda-sort-view-validate-strategy (strategy user-defined-sorter)
  (let ((length (agenda-proper-list-length strategy)))
    (unless (and (integerp length)
                 (every (lambda (criterion)
                          (and (keywordp criterion)
                               (member criterion +agenda-sort-view-strategies+
                                       :test #'eq)))
                        strategy))
      (agenda-sort-view-invalid-option
       strategy "Sorting strategy must be a proper list of supported keywords")))
  (when (and (member :user-defined-up strategy :test #'eq)
             (not (functionp user-defined-sorter)))
    (agenda-sort-view-invalid-option
     strategy "USER-DEFINED-UP requires a user-defined sorter function"))
  (when (and (member :user-defined-down strategy :test #'eq)
             (not (functionp user-defined-sorter)))
    (agenda-sort-view-invalid-option
     strategy "USER-DEFINED-DOWN requires a user-defined sorter function"))
  strategy)

(defun agenda-sort-view-validate-entries (entries)
  (unless (typep entries 'sequence)
    (agenda-signal-error :unsupported-entry entries
                         "Sortable agenda entries must be a sequence"))
  (when (and (listp entries)
             (not (integerp (agenda-proper-list-length entries))))
    (agenda-signal-error :unsupported-entry entries
                         "Sortable agenda entries must be a proper sequence"))
  (map nil
       (lambda (entry)
         (unless (or (agenda-entry-p entry) (agenda-match-entry-p entry))
           (agenda-signal-error :unsupported-entry entry
                                "Sort entries must be agenda or match entries")))
       entries)
  entries)

(defun agenda-sort-view-entry-node (entry)
  (if (agenda-entry-p entry)
      (loop for node = (agenda-entry-source-node entry) then (node-parent node)
            while node
            when (member (node-type node) '(:headline :inlinetask) :test #'eq)
              return node)
      (agenda-match-entry-node entry)))

(defun agenda-sort-view-entry-document (entry)
  (if (agenda-match-entry-p entry)
      (agenda-match-entry-document entry)
      (let ((node (agenda-sort-view-entry-node entry)))
        (and node (node-document node)))))

(defun agenda-sort-view-entry-headline (entry)
  (if (agenda-entry-p entry)
      (agenda-entry-headline entry)
      (let ((node (agenda-sort-view-entry-node entry)))
        (and node (node-property node :raw-value)))))

(defun agenda-sort-view-entry-category (entry)
  (if (agenda-entry-p entry)
      (agenda-entry-category entry)
      (let ((node (agenda-sort-view-entry-node entry)))
        (and node (agenda-heading-category
                   (agenda-sort-view-entry-document entry) node)))))

(defun agenda-sort-view-entry-tags (entry)
  (if (agenda-entry-p entry)
      (agenda-entry-effective-tags entry)
      (let ((node (agenda-sort-view-entry-node entry)))
        (and node (effective-heading-tags
                   (agenda-sort-view-entry-document entry) node)))))

(defun agenda-sort-view-entry-todo-state (entry)
  (if (agenda-entry-p entry)
      (agenda-entry-todo-state entry)
      (let ((node (agenda-sort-view-entry-node entry)))
        (and node (node-property node :todo-keyword)))))

(defun agenda-sort-view-entry-planning-node (entry)
  (let ((heading (agenda-sort-view-entry-node entry)))
    (loop for section in (and heading (node-children heading))
          thereis (and (eq (node-type section) :section)
                       (find :planning (node-children section)
                             :key #'node-type)))))

(defun agenda-sort-view-entry-timestamp-node (entry kind)
  (let ((heading (agenda-sort-view-entry-node entry))
        (planning (agenda-sort-view-entry-planning-node entry)))
    (labels ((property-timestamp (property timestamp-kind)
               (let ((value (node-property property :value)))
                 (when (stringp value)
                   (loop with position = 0
                         for opening = (position-if
                                        (lambda (character)
                                          (find character '(#\< #\[)))
                                        value :start position)
                         while opening
                         do (multiple-value-bind (fields end)
                                (%parse-single-timestamp value opening
                                                         (length value))
                              (declare (ignore end))
                              (when (and fields
                                         (if (eq timestamp-kind :active)
                                             (member
                                              (%timestamp-fields-type fields)
                                              '(:active :active-range) :test #'eq)
                                             (member
                                              (%timestamp-fields-type fields)
                                              '(:inactive :inactive-range) :test #'eq)))
                                (return
                                  (make-node
                                   :timestamp
                                   :properties (%timestamp-properties fields))))
                              (setf position (1+ opening)))))))
             (find-timestamp (node timestamp-kind)
               (when (and node
                          (not (eq node planning))
                          (or (eq node heading)
                              (not (member (node-type node)
                                           '(:headline :inlinetask) :test #'eq))))
                 (or (and (eq (node-type node) :timestamp)
                          (let ((type (node-property node :type)))
                            (when (if (eq timestamp-kind :active)
                                      (member type '(:active :active-range) :test #'eq)
                                      (member type '(:inactive :inactive-range) :test #'eq))
                              node)))
                     (and (eq (node-type node) :node-property)
                          (property-timestamp node timestamp-kind))
                     (loop for child in (node-children node)
                           thereis (find-timestamp child timestamp-kind))))))
      (case kind
        (:scheduled (and planning (node-property planning :scheduled)))
        (:deadline (and planning (node-property planning :deadline)))
        (:active (find-timestamp heading :active))
        (:inactive (find-timestamp heading :inactive))
        (:any
         (or (and planning (node-property planning :scheduled))
             (and planning (node-property planning :deadline))
             (find-timestamp heading :active)
             (find-timestamp heading :inactive)))))))

(defun agenda-sort-view-node-timestamp (node)
  (when (and (node-p node) (eq (node-type node) :timestamp))
    (timestamp-view node)))

(defun agenda-sort-view-entry-timestamp (entry kind)
  (if (agenda-match-entry-p entry)
      (agenda-sort-view-node-timestamp
       (agenda-sort-view-entry-timestamp-node entry kind))
      (let* ((source-node (agenda-entry-source-node entry))
             (entry-kind (agenda-entry-kind entry))
             (source-kind (and (node-p source-node)
                               (eq (node-type source-node) :timestamp)
                               (case (node-property source-node :type)
                                 ((:active :active-range) :active)
                                 ((:inactive :inactive-range) :inactive))))
             (view (or (agenda-entry-timestamp entry)
                       (agenda-sort-view-node-timestamp source-node))))
        (when (or (eq kind :any)
                  (and (eq kind :scheduled) (eq entry-kind :scheduled))
                  (and (eq kind :deadline) (eq entry-kind :deadline))
                  (and (eq kind :active) (eq source-kind :active))
                  (and (eq kind :inactive) (eq source-kind :inactive)))
          view))))

(defun agenda-sort-view-date-key (value)
  (when (org-time-value-p value)
    (cl-date-kit:local-date-to-epoch-day
     (cl-date-kit:make-local-date (org-time-value-year value)
                                  (org-time-value-month value)
                                  (org-time-value-day value)))))

(defun agenda-sort-view-timestamp-date-key (view)
  (let ((start (and (org-timestamp-view-p view)
                    (org-timestamp-view-start view))))
    (agenda-sort-view-date-key start)))

(defun agenda-sort-view-time-key (entry)
  (if (agenda-entry-p entry)
      (let* ((view (agenda-sort-view-entry-timestamp entry :any))
             (start (and (org-timestamp-view-p view)
                         (org-timestamp-view-start view)))
             (hour (and start (org-time-value-hour start)))
             (minute (and start (org-time-value-minute start))))
        (if (and hour minute)
            (+ (* 100 hour) minute)
            9901))
      (or (agenda-sort-view-match-entry-time-key entry)
          9901)))
(progn
  (defun agenda-sort-view-ascii-digit-p (character)
    (and character (char<= #\0 character #\9)))

  (defun agenda-sort-view-time-timestamp-end (text position)
    (let* ((text-length (length text))
           (opener (and (< position text-length) (char text position)))
           (start (1+ position))
           (after-date (+ start 10)))
      (when (member opener '(#\< #\[))
        (when (and (<= after-date text-length)
                   (loop for cursor from start below (+ start 4)
                         always
                         (agenda-sort-view-ascii-digit-p (char text cursor)))
                   (char= (char text (+ start 4)) #\-)
                   (agenda-sort-view-ascii-digit-p (char text (+ start 5)))
                   (agenda-sort-view-ascii-digit-p (char text (+ start 6)))
                   (char= (char text (+ start 7)) #\-)
                   (agenda-sort-view-ascii-digit-p (char text (+ start 8)))
                   (agenda-sort-view-ascii-digit-p (char text (+ start 9)))
                   (or (= after-date text-length)
                       (char= (char text after-date) #\Space)
                       (member (char text after-date) '(#\] #\>))))
          (loop for cursor from after-date below text-length
                for character = (char text cursor)
                when (member character '(#\] #\>))
                  return (1+ cursor)
                when (member character '(#\Newline #\Return))
                  return nil)))))

  (defun agenda-sort-view-time-suffix-at (text position)
    (when (<= (+ position 2) (length text))
      (let ((suffix (string-downcase (subseq text position (+ position 2)))))
        (when (member suffix '("am" "pm") :test #'string=)
          suffix))))

  (defun agenda-sort-view-plain-time-at (text position)
    (let ((text-length (length text)))
      (when (or (zerop position)
                (not (alphanumericp (char text (1- position)))))
        (let ((first-hour-digit
                (and (< position text-length)
                     (digit-char-p (char text position) 10))))
          (when first-hour-digit
            (let* ((second-position (1+ position))
                   (two-digit-hour
                     (and (< second-position text-length)
                          (<= first-hour-digit 2)
                          (agenda-sort-view-ascii-digit-p
                           (char text second-position))))
                   (hour-end (if two-digit-hour (+ position 2) (1+ position)))
                   (hour (parse-integer text :start position :end hour-end))
                   (cursor hour-end)
                   (minute 0)
                   has-minute
                   suffix)
              (if (and (< cursor text-length)
                       (char= (char text cursor) #\:)
                       (<= (+ cursor 3) text-length)
                       (agenda-sort-view-ascii-digit-p
                        (char text (1+ cursor)))
                       (agenda-sort-view-ascii-digit-p
                        (char text (+ cursor 2)))
                       (<= (parse-integer text
                                          :start (1+ cursor)
                                          :end (+ cursor 3))
                           59))
                  (progn
                    (setf minute
                          (parse-integer text
                                         :start (1+ cursor)
                                         :end (+ cursor 3))
                          cursor (+ cursor 3)
                          has-minute t)
                    (setf suffix
                          (agenda-sort-view-time-suffix-at text cursor))
                    (when suffix (incf cursor 2)))
                  (progn
                    (setf suffix
                          (agenda-sort-view-time-suffix-at text cursor))
                    (when suffix (incf cursor 2))))
              (when (and (or has-minute suffix)
                         (or (= cursor text-length)
                             (not (alphanumericp (char text cursor)))))
                (when (and suffix (string= suffix "pm") (/= hour 12))
                  (incf hour 12))
                (when (and suffix (string= suffix "am") (= hour 12))
                  (setf hour 0))
                (+ (* hour 100) minute))))))))

  (defun agenda-sort-view-match-entry-time-key (entry)
    (let* ((text (or (agenda-sort-view-entry-headline entry) ""))
           (text-length (length text)))
      (loop for position from 0 below text-length
            for timestamp-end =
              (agenda-sort-view-time-timestamp-end text position)
            do (if timestamp-end
                   (setf position (1- timestamp-end))
                   (let ((time (agenda-sort-view-plain-time-at text position)))
                     (when time
                       (return-from agenda-sort-view-match-entry-time-key time))))
            finally (return nil)))))

(defun agenda-sort-view-compare-number (left right direction)
  (case direction
    (:up (cond ((< left right) -1)
               ((> left right) 1)))
    (:down (cond ((> left right) -1)
                 ((< left right) 1)))))

(defun agenda-sort-view-compare-strings (left right direction &key downcase)
  (let ((left (and left (if downcase (string-downcase left) left)))
        (right (and right (if downcase (string-downcase right) right))))
    (let ((ascending
            (cond ((and (null left) (null right)) nil)
                  ((null left) 1)
                  ((null right) -1)
                  ((string< left right) -1)
                  ((string< right left) 1))))
      (if (and ascending (eq direction :down))
          (- ascending)
          ascending))))

(defun agenda-sort-view-compare-tag (left right direction)
  (let* ((left-tag (car (last (agenda-sort-view-entry-tags left))))
         (right-tag (car (last (agenda-sort-view-entry-tags right)))))
    (agenda-sort-view-compare-strings left-tag right-tag direction)))

(defun agenda-sort-view-compare-timestamp (left right direction)
  (agenda-sort-view-compare-number
   (or (agenda-sort-view-timestamp-date-key left) most-positive-fixnum)
   (or (agenda-sort-view-timestamp-date-key right) most-positive-fixnum)
   direction))

(defun agenda-sort-view-entry-priority (entry)
  (let ((node (agenda-sort-view-entry-node entry))
        (document (agenda-sort-view-entry-document entry)))
    (if (and node document)
        (multiple-value-bind (highest lowest default kind)
            (edit-priority-settings document nil)
          (declare (ignore highest kind))
          (* 1000 (- lowest (or (node-property node :priority) default))))
        1000)))
(defun agenda-sort-view-entry-statistics (entry)
  (labels ((number-or-zero (text start end)
             (if (< start end)
                 (or (parse-integer text :start start :end end :junk-allowed t) 0)
                 0))
           (cookie-value (cookie)
             (let* ((text (node-property cookie :value))
                    (start (and (stringp text) (position #\[ text)))
                    (end (and start (position #\] text :from-end t)))
                    (separator
                      (and start end
                           (position-if
                            (lambda (character)
                              (find character '(#\/ #\%)))
                            text :start (1+ start) :end end))))
               (if separator
                   (let ((numerator (number-or-zero text (1+ start) separator)))
                     (if (char= (char text separator) #\/)
                         (let ((denominator
                                 (number-or-zero text (1+ separator) end)))
                           (if (zerop denominator)
                               0
                               (floor (* numerator 100) denominator)))
                         numerator))
                   0))))
    (loop for heading = (agenda-sort-view-entry-node entry)
            then (node-parent heading)
          while heading
          when (member (node-type heading) '(:headline :inlinetask) :test #'eq)
            do (let ((cookies
                       (remove-if-not
                        (lambda (node)
                          (eq (node-type node) :statistics-cookie))
                        (statistics-cookie-title-nodes heading))))
                 (when cookies
                   (return-from agenda-sort-view-entry-statistics
                     (cookie-value (car (last cookies))))))
          finally (return 0))))

(defun agenda-sort-view-entry-effort (entry)
  (let ((node (agenda-sort-view-entry-node entry)))
    (if node
        (multiple-value-bind (value found-p)
            (effective-heading-property-value node "EFFORT")
          (if found-p
              (effort-duration-minutes value :limits
                                       (document-limits
                                        (agenda-sort-view-entry-document entry)))
              most-positive-fixnum))
        most-positive-fixnum)))

(defun agenda-sort-view-entry-todo-rank (entry)
  (let* ((state (agenda-sort-view-entry-todo-state entry))
         (document (agenda-sort-view-entry-document entry))
         (keywords (and document
                        (getf (document-settings document) :todo-keywords)))
         (names (mapcar (lambda (keyword)
                          (cond ((stringp keyword) keyword)
                                ((and (consp keyword) (stringp (car keyword)))
                                 (car keyword))
                                (t (query-todo-sequence-keyword-name keyword))))
                        keywords))
         (remaining (and (stringp state)
                         (member state names :test #'string=))))
    (- (length remaining))))

(defun agenda-sort-view-entry-done-p (entry)
  (let* ((node (agenda-sort-view-entry-node entry))
         (state (agenda-sort-view-entry-todo-state entry))
         (document (agenda-sort-view-entry-document entry)))
    (or (and node (eq (node-property node :todo-type) :done))
        (and (stringp state) document
             (query-done-keyword-p state document)))))

(defun agenda-sort-view-compare-todo-state (left right direction)
  (declare (ignore direction))
  (let ((left-done-p (agenda-sort-view-entry-done-p left))
        (right-done-p (agenda-sort-view-entry-done-p right))
        (left-rank (agenda-sort-view-entry-todo-rank left))
        (right-rank (agenda-sort-view-entry-todo-rank right)))
    (cond ((and left-done-p (not right-done-p)) -1)
          ((and right-done-p (not left-done-p)) 1)
          ((< left-rank right-rank) -1)
          ((> left-rank right-rank) 1))))

(defun agenda-sort-view-entry-habit-p (entry)
  (when (and (agenda-entry-p entry)
             (member (agenda-entry-kind entry) '(:scheduled :timestamp)
                     :test #'eq))
    (or (agenda-entry-habit-p entry)
        (let ((node (agenda-sort-view-entry-node entry)))
          (and node
               (multiple-value-bind (style found-p)
                   (effective-property (agenda-sort-view-entry-document entry)
                                       node "STYLE")
                 (and found-p (stringp style)
                      (string-equal style "habit"))))))))

(defun agenda-sort-view-compare-habit (left right direction)
  (let ((left-p (not (null (agenda-sort-view-entry-habit-p left))))
        (right-p (not (null (agenda-sort-view-entry-habit-p right)))))
    (cond ((and left-p (not right-p))
           (if (eq direction :up) -1 1))
          ((and right-p (not left-p))
           (if (eq direction :up) 1 -1)))))

(defun agenda-sort-view-call-user-sorter (sorter left right)
  (let ((result (funcall sorter left right)))
    (unless (or (null result) (eql result -1) (eql result 1))
      (agenda-sort-view-invalid-option
       result "User-defined sorter must return -1, 1, or NIL"))
    result))

(defun agenda-sort-view-entry-urgency (entry time-context)
  (if (agenda-match-entry-p entry)
      (1+ (agenda-sort-view-entry-priority entry))
      (let ((priority (agenda-sort-view-entry-priority entry))
            (kind (agenda-entry-kind entry)))
        (case kind
          (:todo (1+ priority))
          (:scheduled
           (if (agenda-sort-view-entry-habit-p entry)
               (agenda-sort-view-habit-urgency entry time-context)
               (let* ((view (agenda-sort-view-entry-timestamp entry :scheduled))
                      (schedule (agenda-sort-view-timestamp-date-key view))
                      (date (agenda-sort-view-date-key (agenda-entry-date entry))))
                 (unless (and schedule date)
                   (agenda-signal-error
                    :unsupported-entry entry
                    "Scheduled urgency requires a scheduled timestamp and row date"))
                 (+ 99 (- date schedule) priority))))
          (:deadline
           (let* ((view (agenda-sort-view-entry-timestamp entry :deadline))
                  (deadline (agenda-sort-view-timestamp-date-key view))
                  (date (agenda-sort-view-date-key (agenda-entry-date entry)))
                  (today (agenda-date-ordinal
                          (agenda-reference-date time-context))))
             (unless (and deadline date)
               (agenda-signal-error
                :unsupported-entry entry
                "Deadline urgency requires a deadline timestamp and row date"))
             (if (= date today)
                 (- priority (- deadline date))
                 priority)))
          (otherwise
           (if (agenda-sort-view-entry-habit-p entry)
               (agenda-sort-view-habit-urgency entry time-context)
               priority))))))

(defun agenda-sort-view-habit-urgency (entry time-context)
  (let* ((planning (agenda-sort-view-entry-planning-node entry))
         (scheduled-node (and planning (node-property planning :scheduled)))
         (view (agenda-sort-view-node-timestamp scheduled-node))
         (window-value (and scheduled-node
                            (node-property scheduled-node
                                           :repeater-window-value)))
         (window-unit (and scheduled-node
                           (node-property scheduled-node
                                          :repeater-window-unit))))
    (unless (and view
                 (org-timestamp-view-repeater-amount view)
                 (agenda-sort-view-date-key (org-timestamp-view-start view)))
      (agenda-signal-error :unsupported-entry entry
                           "Habit urgency requires a scheduled timestamp with a repeater"))
    (let* ((scheduled (agenda-sort-view-timestamp-date-key view))
           (repeat-days (agenda-habit-repeat-days view))
           (window-days (and window-value
                             (agenda-habit-duration-days
                              window-value window-unit)))
           (today (agenda-date-ordinal
                   (agenda-reference-date time-context)))
           (deadline (if window-days
                         (+ scheduled (- window-days repeat-days))
                         scheduled)))
      (unless (plusp repeat-days)
        (agenda-signal-error
         :unsupported-entry entry
         "Habit urgency requires a repeat period of at least one day"))
      (when (and window-days (<= window-days repeat-days))
        (agenda-signal-error
         :unsupported-entry entry
         "Habit urgency requires a repeat window longer than its repeat period"))
      (let* ((urgency (+ 1000 (* (- today scheduled) 10)))
             (urgency
               (if (and (/= scheduled deadline) (= today deadline))
                   (+ urgency 50)
                   urgency))
             (slip (- today (1- deadline))))
        (if (plusp slip)
            (+ urgency (* slip 100))
            (+ urgency (* slip 10)))))))

(defun agenda-sort-view-compare-criterion (left right criterion sorter time-context)
  (case criterion
    ((:time-up :time-down)
     (agenda-sort-view-compare-number
      (agenda-sort-view-time-key left)
      (agenda-sort-view-time-key right)
      (if (eq criterion :time-up) :up :down)))
    ((:timestamp-up :timestamp-down)
     (agenda-sort-view-compare-timestamp
      (agenda-sort-view-entry-timestamp left :any)
      (agenda-sort-view-entry-timestamp right :any)
      (if (eq criterion :timestamp-up) :up :down)))
    ((:scheduled-up :scheduled-down)
     (agenda-sort-view-compare-timestamp
      (agenda-sort-view-entry-timestamp left :scheduled)
      (agenda-sort-view-entry-timestamp right :scheduled)
      (if (eq criterion :scheduled-up) :up :down)))
    ((:deadline-up :deadline-down)
     (agenda-sort-view-compare-timestamp
      (agenda-sort-view-entry-timestamp left :deadline)
      (agenda-sort-view-entry-timestamp right :deadline)
      (if (eq criterion :deadline-up) :up :down)))
    ((:ts-up :ts-down)
     (agenda-sort-view-compare-timestamp
      (agenda-sort-view-entry-timestamp left :active)
      (agenda-sort-view-entry-timestamp right :active)
      (if (eq criterion :ts-up) :up :down)))
    ((:tsia-up :tsia-down)
     (agenda-sort-view-compare-timestamp
      (agenda-sort-view-entry-timestamp left :inactive)
      (agenda-sort-view-entry-timestamp right :inactive)
      (if (eq criterion :tsia-up) :up :down)))
    (:category-keep
     (unless (equal (agenda-sort-view-entry-category left)
                    (agenda-sort-view-entry-category right))
       :stop))
    ((:category-up :category-down)
     (agenda-sort-view-compare-strings
      (or (agenda-sort-view-entry-category left) "")
      (or (agenda-sort-view-entry-category right) "")
      (if (eq criterion :category-up) :up :down)))
    ((:tag-up :tag-down)
     (agenda-sort-view-compare-tag
      left right (if (eq criterion :tag-up) :up :down)))
    ((:priority-up :priority-down)
     (agenda-sort-view-compare-number
      (agenda-sort-view-entry-priority left)
      (agenda-sort-view-entry-priority right)
      (if (eq criterion :priority-up) :up :down)))
    ((:stats-up :stats-down)
     (agenda-sort-view-compare-number
      (agenda-sort-view-entry-statistics left)
      (agenda-sort-view-entry-statistics right)
      (if (eq criterion :stats-up) :up :down)))
    ((:urgency-up :urgency-down)
     (agenda-sort-view-compare-number
      (agenda-sort-view-entry-urgency left time-context)
      (agenda-sort-view-entry-urgency right time-context)
      (if (eq criterion :urgency-up) :up :down)))
    ((:todo-state-up :todo-state-down)
     (agenda-sort-view-compare-todo-state
      left right (if (eq criterion :todo-state-up) :up :down)))
    ((:effort-up :effort-down)
     (agenda-sort-view-compare-number
      (agenda-sort-view-entry-effort left)
      (agenda-sort-view-entry-effort right)
      (if (eq criterion :effort-up) :up :down)))
    ((:habit-up :habit-down)
     (agenda-sort-view-compare-habit
      left right (if (eq criterion :habit-up) :up :down)))
    ((:alpha-up :alpha-down)
     (agenda-sort-view-compare-strings
      (agenda-sort-view-entry-headline left)
      (agenda-sort-view-entry-headline right)
      (if (eq criterion :alpha-up) :up :down) :downcase t))
    (:user-defined-up
     (let ((result (agenda-sort-view-call-user-sorter sorter left right)))
       (cond ((eql result -1) -1)
             ((eql result 1) 1))))
    (:user-defined-down
     (let ((result (agenda-sort-view-call-user-sorter sorter right left)))
       (cond ((eql result -1) -1)
             ((eql result 1) 1))))))

(defun agenda-sort-view-compare-entries (left right strategy sorter time-context)
  (dolist (criterion strategy)
    (let ((result (agenda-sort-view-compare-criterion
                   left right criterion sorter time-context)))
      (when (eq result :stop)
        (return-from agenda-sort-view-compare-entries 0))
      (when result
        (return-from agenda-sort-view-compare-entries result))))
  0)

(defun agenda-sort-view-validate-time-context (time-context)
  (unless time-context
    (agenda-signal-error :time-context-required nil
                         "Urgency sorting requires an explicit time context"))
  (agenda-require-time-context time-context))

(defun agenda-sort-view-entries (entries sorting-strategy
                                &key user-defined-sorter time-context)
  (when (null sorting-strategy)
    (return-from agenda-sort-view-entries entries))
  (agenda-sort-view-validate-strategy sorting-strategy user-defined-sorter)
  (agenda-sort-view-validate-entries entries)
  (when (or (member :urgency-up sorting-strategy :test #'eq)
            (member :urgency-down sorting-strategy :test #'eq))
    (agenda-sort-view-validate-time-context time-context))
  (let ((groups-by-date (make-hash-table :test #'eql))
        (groups nil))
    (map nil
         (lambda (entry)
           (let ((date (and (agenda-entry-p entry)
                            (agenda-sort-view-date-key
                             (agenda-entry-date entry)))))
             (multiple-value-bind (group present-p)
                 (gethash date groups-by-date)
               (unless present-p
                 (setf group (cons date nil)
                       (gethash date groups-by-date) group)
                 (push group groups))
               (push entry (cdr group)))))
         entries)
    (let ((sorted
            (mapcan
             (lambda (group)
               (stable-sort
                (nreverse (cdr group))
                (lambda (left right)
                  (minusp
                   (agenda-sort-view-compare-entries
                    left right sorting-strategy user-defined-sorter
                    time-context)))))
             (nreverse groups))))
      (if (listp entries)
          sorted
          (let ((copy (copy-seq entries)))
            (loop for entry in sorted
                  for index from 0
                  do (setf (aref copy index) entry))
            copy)))))

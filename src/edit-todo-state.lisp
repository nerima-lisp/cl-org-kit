;;;; src/edit-todo-state.lisp
;;;; High-level TODO workflow transitions.
(in-package #:org-kit)

(defstruct (org-workflow-options
            (:constructor %make-org-workflow-options
                (&key log-done log-repeat repeat-to-state
                      (log-into-drawer "LOGBOOK")
                      (clock-into-drawer "LOGBOOK")))
            (:conc-name %org-workflow-options-)
            (:copier nil)
            (:predicate %org-workflow-options-p))
  "Workflow settings that override document-wide TODO logging settings."
  (log-done nil :read-only t)
  (log-repeat nil :read-only t)
  (repeat-to-state nil :read-only t)
  (log-into-drawer "LOGBOOK" :read-only t)
  (clock-into-drawer "LOGBOOK" :read-only t))

(defun make-org-workflow-options (&key log-done log-repeat repeat-to-state
                                    (log-into-drawer "LOGBOOK")
                                    (clock-into-drawer "LOGBOOK"))
  "Make explicit TODO workflow settings with kit defaults for both drawers."
  (%make-org-workflow-options :log-done log-done :log-repeat log-repeat
                              :repeat-to-state repeat-to-state
                              :log-into-drawer log-into-drawer
                              :clock-into-drawer clock-into-drawer))

(defun org-workflow-options-p (object)
  "Return true when OBJECT is an Org workflow options value."
  (%org-workflow-options-p object))

(defun org-workflow-options-log-done (options)
  "Return OPTIONS' log-done setting."
  (%org-workflow-options-log-done options))

(defun org-workflow-options-log-repeat (options)
  "Return OPTIONS' log-repeat setting."
  (%org-workflow-options-log-repeat options))

(defun org-workflow-options-repeat-to-state (options)
  "Return OPTIONS' repeat-to-state keyword."
  (%org-workflow-options-repeat-to-state options))

(defun org-workflow-options-log-into-drawer (options)
  "Return OPTIONS' log drawer name."
  (%org-workflow-options-log-into-drawer options))

(defun org-workflow-options-clock-into-drawer (options)
  "Return OPTIONS' clock drawer setting."
  (%org-workflow-options-clock-into-drawer options))

(defstruct (transition-result
            (:constructor %make-transition-result
                (&key previous-state new-state log-events
                      rescheduled-timestamps closed-timestamp))
            (:conc-name %transition-result-)
            (:copier nil)
            (:predicate %transition-result-p))
  "The state, log lines, and timestamps produced by a TODO transition."
  (previous-state nil :read-only t)
  (new-state nil :read-only t)
  (log-events nil :read-only t)
  (rescheduled-timestamps nil :read-only t)
  (closed-timestamp nil :read-only t))

(defun make-transition-result (&key previous-state new-state log-events
                                 rescheduled-timestamps closed-timestamp)
  "Make a TODO transition result from its states, log lines, and timestamps."
  (%make-transition-result :previous-state previous-state :new-state new-state
                           :log-events log-events
                           :rescheduled-timestamps rescheduled-timestamps
                           :closed-timestamp closed-timestamp))

(defun transition-result-p (object)
  "Return true when OBJECT is a TODO transition result."
  (%transition-result-p object))

(defun transition-result-previous-state (result)
  "Return RESULT's TODO state before the transition, or NIL."
  (%transition-result-previous-state result))

(defun transition-result-new-state (result)
  "Return RESULT's TODO state after the transition, or NIL."
  (%transition-result-new-state result))

(defun transition-result-log-events (result)
  "Return RESULT's inserted log lines without their line terminators."
  (%transition-result-log-events result))

(defun transition-result-rescheduled-timestamps (result)
  "Return complete source strings for timestamps rescheduled by RESULT."
  (%transition-result-rescheduled-timestamps result))

(defun transition-result-closed-timestamp (result)
  "Return the resulting CLOSED timestamp string, or NIL when none remains."
  (%transition-result-closed-timestamp result))

(defun edit-todo-error (target reason message)
  (edit-signal-error target reason message))

(defun edit-todo-active-sequences (document target)
  (let ((sequences (getf (document-settings document) :todo-sequences)))
    (unless (reader-heading-normalized-todo-sequences-p sequences)
      (edit-todo-error target :invalid-value
                       "Document has no normalized TODO sequences"))
    (reader-heading-active-todo-sequences sequences)))

(defun edit-todo-sequence-states (sequence)
  (append (getf sequence :todo-keywords)
          (getf sequence :done-keywords)))

(defun edit-todo-sequence-state (sequence state)
  (find state (edit-todo-sequence-states sequence)
        :key (lambda (entry) (getf entry :keyword))
        :test #'string=))

(defun edit-todo-sequence-for-state (sequences state target)
  (or (find-if (lambda (sequence)
                 (edit-todo-sequence-state sequence state))
               sequences)
      (edit-todo-error target :invalid-value
                       "TODO state is not configured in an active sequence")))

(defun edit-todo-sequence-by-id (sequences sequence-id target)
  (or (find sequence-id sequences :key (lambda (sequence)
                                         (getf sequence :sequence-id))
             :test #'equal)
      (edit-todo-error target :invalid-value
                       "TODO sequence ID is not configured")))

(defun edit-todo-select-cycle-sequence
    (document heading sequence-id sequence-id-supplied-p target)
  (let* ((sequences (edit-todo-active-sequences document target))
         (current (node-property heading :todo-keyword)))
    (if sequence-id-supplied-p
        (let ((sequence (edit-todo-sequence-by-id sequences sequence-id target)))
          (when (and current (not (edit-todo-sequence-state sequence current)))
            (edit-todo-error target :invalid-value
                             "Current TODO state is outside the selected sequence"))
          sequence)
        (cond
          (current (edit-todo-sequence-for-state sequences current target))
          ((null (cdr sequences)) (first sequences))
          (t (edit-todo-error target :ambiguous-target
                              "TODO sequence must be selected for a heading without state"))))))

(defun edit-todo-cycle-state (sequence current direction target)
  (let* ((todos (getf sequence :todo-keywords))
         (dones (getf sequence :done-keywords))
         (states (edit-todo-sequence-states sequence))
         (index (and current
                     (position current states
                               :key (lambda (entry) (getf entry :keyword))
                               :test #'string=))))
    (cond
      ((null current)
       (or (getf (if (eq direction :next) (first todos) (car (last dones)))
                 :keyword)
           (edit-todo-error target :invalid-value
                            "TODO sequence has no state in the requested direction")))
      ((eq direction :next)
       (if (= index (1- (length states)))
           nil
           (getf (nth (1+ index) states) :keyword)))
      (t
       (if (zerop index)
           nil
           (getf (nth (1- index) states) :keyword))))))

(defun cycle-heading-todo
    (document target &key direction (sequence-id nil sequence-id-supplied-p)
                         (at nil at-supplied-p) time-context workflow-options note)
  "Cycle TARGET through one TODO sequence and return edit and transition results.
DIRECTION is :NEXT or :PREVIOUS; an omitted SEQUENCE-ID must be unambiguous."
  (unless (member direction '(:next :previous))
    (edit-todo-error target :invalid-value "TODO cycle direction must be :NEXT or :PREVIOUS"))
  (let* ((heading (edit-headline-resolve-target document target))
         (sequence
           (edit-todo-select-cycle-sequence
            document heading sequence-id sequence-id-supplied-p heading))
         (state (edit-todo-cycle-state
                 sequence (node-property heading :todo-keyword) direction heading)))
    (if at-supplied-p
        (transition-heading-todo document heading state
                                 :at at :time-context time-context
                                 :workflow-options workflow-options :note note)
        (transition-heading-todo document heading state
                                 :time-context time-context
                                 :workflow-options workflow-options :note note))))

(defun edit-todo-normalize-at (at)
  (cond ((org-time-value-p at) at)
        ((stringp at) (make-org-time-value at))
        ((cl-date-kit:local-date-p at) (%time-value-from-local-value at))
        ((cl-date-kit:local-date-time-p at)
         (%time-value-from-local-value at))
        (t (%org-time-error :invalid-time at))))

(defun edit-todo-context-zone (time-context)
  (unless (org-time-context-p time-context)
    (%org-time-error :invalid-time "date-time repeater requires a time context"))
  (org-time-context-zone time-context))

(defun edit-todo-localize-time-value (value time-context)
  (if (org-time-value-offset-minutes value)
      (let* ((zone (edit-todo-context-zone time-context))
             (instant (%time-value-to-instant
                       value zone (org-time-context-dst-policy time-context)))
             (local (cl-date-kit:local-date-time-of-instant
                     instant (org-time-zone-backend zone))))
        (%time-value-from-local-value local))
      value))

(defun edit-todo-value-order-key (value)
  (+ (* 86400
        (cl-date-kit:local-date-to-epoch-day
         (cl-date-kit:make-local-date (org-time-value-year value)
                                      (org-time-value-month value)
                                      (org-time-value-day value))))
     (* 3600 (or (org-time-value-hour value) 0))
     (* 60 (or (org-time-value-minute value) 0))
     (or (org-time-value-second value) 0)))

(defun edit-todo-value-after-p (left right time-context)
  (if (and time-context
           (org-time-context-p time-context)
           (eq (org-time-value-kind left) :date-time)
           (eq (org-time-value-kind right) :date-time))
      (> (cl-date-kit:instant-epoch-second
          (%time-value-to-instant
           left (org-time-context-zone time-context)
           (org-time-context-dst-policy time-context)))
         (cl-date-kit:instant-epoch-second
          (%time-value-to-instant
           right (org-time-context-zone time-context)
           (org-time-context-dst-policy time-context))))
      (> (edit-todo-value-order-key
          (edit-todo-localize-time-value left time-context))
         (edit-todo-value-order-key
          (edit-todo-localize-time-value right time-context)))))

(defun edit-todo-local-value-plus (value amount unit)
  (let* ((date (cl-date-kit:make-local-date
                (org-time-value-year value)
                (org-time-value-month value)
                (org-time-value-day value)))
         (time (and (eq (org-time-value-kind value) :date-time)
                    (cl-date-kit:make-local-time
                     (org-time-value-hour value)
                     (org-time-value-minute value)
                     (or (org-time-value-second value) 0))))
         (local (if time (cl-date-kit:make-local-date-time date time) date))
         (updated
           (case unit
             (:day (if time (cl-date-kit:local-date-time-plus-days local amount)
                       (cl-date-kit:local-date-plus-days local amount)))
             (:week (if time (cl-date-kit:local-date-time-plus-weeks local amount)
                        (cl-date-kit:local-date-plus-weeks local amount)))
             (:month (if time (cl-date-kit:local-date-time-plus-months local amount)
                         (cl-date-kit:local-date-plus-months local amount)))
             (:year (if time (cl-date-kit:local-date-time-plus-years local amount)
                        (cl-date-kit:local-date-plus-years local amount))))))
    (%time-value-from-local-value updated)))

(defun edit-todo-value-plus-hours (value amount time-context)
  (let* ((zone (edit-todo-context-zone time-context))
         (instant (%time-value-to-instant
                   value zone (org-time-context-dst-policy time-context)))
         (later (cl-date-kit:instant-plus-hours instant amount)))
    (%time-value-from-local-value
     (cl-date-kit:local-date-time-of-instant
      later (org-time-zone-backend zone)))))

(defun edit-todo-add-repeater (value amount unit time-context)
  (if (eq unit :hour)
      (edit-todo-value-plus-hours value amount time-context)
      (progn
        (when (and (eq (org-time-value-kind value) :date-time)
                   (not time-context))
          (%org-time-error :invalid-time
                           "date-time repeater requires a time context"))
      (let* ((zone (and time-context (edit-todo-context-zone time-context)))
             (updated (edit-todo-local-value-plus value amount unit)))
        (if (and zone (eq (org-time-value-kind updated) :date-time))
            (let ((instant
                    (%time-value-to-instant
                     updated zone (org-time-context-dst-policy time-context))))
              (%time-value-from-local-value
               (cl-date-kit:local-date-time-of-instant
                instant (org-time-zone-backend zone))))
            updated)))))

(defun edit-todo-restart-value (value at)
  (let ((at (if (org-time-value-offset-minutes at)
                (make-org-time-value
                 (org-time-value-iso8601 at))
                at)))
    (cond
      ((and (eq (org-time-value-kind value) :date)
            (eq (org-time-value-kind at) :date-time))
       (%time-value-from-local-value
        (cl-date-kit:make-local-date (org-time-value-year at)
                                     (org-time-value-month at)
                                     (org-time-value-day at))))
      ((and (eq (org-time-value-kind value) :date-time)
            (eq (org-time-value-kind at) :date))
       (%time-value-from-local-value
        (cl-date-kit:make-local-date-time
         (cl-date-kit:make-local-date (org-time-value-year at)
                                      (org-time-value-month at)
                                      (org-time-value-day at))
         (cl-date-kit:make-local-time (org-time-value-hour value)
                                      (org-time-value-minute value)
                                      (or (org-time-value-second value) 0)))))
      (t at))))

(defun edit-todo-repeat-value (value at amount unit mode time-context document)
  (let* ((at (if (and (not (eq mode :catch-up))
                      (org-time-value-offset-minutes at)
                      (org-time-context-p time-context))
                 (edit-todo-localize-time-value at time-context)
                 at))
         (base (if (eq mode :restart)
                   (edit-todo-restart-value value at)
                   value)))
    (if (eq mode :catch-up)
        (loop with candidate = (edit-todo-add-repeater
                                base amount unit time-context)
              for count from 1
              when (edit-todo-value-after-p candidate at time-context)
                return (values candidate count)
              when (>= count (limit-value (document-limits document)
                                          :max-future-repeats))
                do (error 'org-resource-limit-error
                          :message "Resource limit exceeded"
                          :limit-name :max-future-repeats
                          :limit (limit-value (document-limits document)
                                              :max-future-repeats)
                          :actual (1+ count))
              do (setf candidate
                       (edit-todo-add-repeater candidate amount unit time-context)))
        (values (edit-todo-add-repeater base amount unit time-context) 1))))

(defun edit-todo-weekday-name (value)
  (cdr (assoc (cl-date-kit:day-of-week
               (cl-date-kit:make-local-date
                (org-time-value-year value)
                (org-time-value-month value)
                (org-time-value-day value)))
              '((:monday . "Mon") (:tuesday . "Tue")
                (:wednesday . "Wed") (:thursday . "Thu")
                (:friday . "Fri") (:saturday . "Sat")
                (:sunday . "Sun")))))

(defun edit-todo-format-point-timestamp (value)
  (let ((year (org-time-value-year value)))
    (unless (<= 0 year 9999)
      (%org-time-error :invalid-time (org-time-value-iso8601 value)))
    (with-output-to-string (stream)
      (format stream "~4,'0D-~2,'0D-~2,'0D ~A"
              year (org-time-value-month value) (org-time-value-day value)
              (edit-todo-weekday-name value))
      (when (eq (org-time-value-kind value) :date-time)
        (format stream " ~2,'0D:~2,'0D"
                (org-time-value-hour value) (org-time-value-minute value))))))

(defun edit-todo-repeater-suffix (view)
  (with-output-to-string (stream)
    (when (org-timestamp-view-repeater-amount view)
      (write-string
       (case (org-timestamp-view-repeater-mode view)
         (:cumulate "+") (:catch-up "++") (:restart ".+")) stream)
      (format stream "~D~C"
              (org-timestamp-view-repeater-amount view)
              (ecase (org-timestamp-view-repeater-unit view)
                (:hour #\h) (:day #\d) (:week #\w)
                (:month #\m) (:year #\y))))
    (when (org-timestamp-view-warning-amount view)
      (write-char #\Space stream)
      (write-string
       (if (eq (org-timestamp-view-warning-mode view) :first) "--" "-")
       stream)
      (format stream "~D~C"
              (org-timestamp-view-warning-amount view)
              (ecase (org-timestamp-view-warning-unit view)
                (:hour #\h) (:day #\d) (:week #\w)
                (:month #\m) (:year #\y)))))
    )

(defun edit-todo-external-range-p (document node)
  (let ((separator (case (node-property node :type)
                     (:active-range ">--<")
                     (:inactive-range "]--["))))
    (and separator
         (search separator (document-text document)
                 :start2 (node-begin node)
                 :end2 (- (node-end node) (node-post-blank node))))))

(defun edit-todo-format-timestamp (view start end time-range-p)
  (let* ((active-p (member (org-timestamp-view-kind view)
                           '(:active :active-range)))
         (open (if active-p #\< #\[))
         (close (if active-p #\> #\]))
         (suffix (edit-todo-repeater-suffix view)))
    (with-output-to-string (stream)
      (write-char open stream)
      (write-string (edit-todo-format-point-timestamp start) stream)
      (when time-range-p
        (format stream "-~2,'0D:~2,'0D"
                (org-time-value-hour end) (org-time-value-minute end)))
      (when (plusp (length suffix))
        (write-char #\Space stream)
        (write-string suffix stream))
      (write-char close stream)
      (when (and end (not time-range-p))
        (write-string "--" stream)
        (write-char open stream)
        (write-string (edit-todo-format-point-timestamp end) stream)
        (write-char close stream)))))

(defun edit-todo-property-value (document heading options key property-name
                                default)
  (multiple-value-bind (value source)
      (effective-property document heading property-name :inherit nil)
    (cond
      (source value)
      (options
       (ecase key
         (:repeat-to-state (org-workflow-options-repeat-to-state options))
         (:log-into-drawer (org-workflow-options-log-into-drawer options))))
      (t (multiple-value-bind (setting present-p)
             (loop for (candidate setting) on (document-settings document) by #'cddr
                   when (eq candidate key)
                     do (return (values setting t))
                   finally (return (values nil nil)))
           (if present-p setting default))))))

(defun edit-todo-normalize-mode (value target)
  (cond
    ((null value) nil)
    ((eq value t) :timestamp)
    ((and (symbolp value)
          (member (string-upcase (symbol-name value))
                  '("TIME" "TIMESTAMP" "STATE")
                  :test #'string=))
     :timestamp)
    ((and (symbolp value)
          (string-equal (symbol-name value) "NOTE"))
     :note)
    ((stringp value)
     (cond ((member (string-upcase value) '("TIME" "TIMESTAMP" "STATE" "T")
                    :test #'string=)
            :timestamp)
           ((string-equal value "NOTE") :note)
           ((member (string-upcase value) '("NIL" "OFF" "FALSE")
                    :test #'string=)
            nil)
           (t (edit-todo-error target :invalid-value
                               "Invalid TODO logging mode"))))
    (t (edit-todo-error target :invalid-value "Invalid TODO logging mode"))))

(defun edit-todo-logging-state (word)
  "Return WORD's (KEYWORD ENTRY-MODE EXIT-MODE) when it is a logging spec such
as WAIT(w@/!), following Org's org-extract-log-state-settings."
  (let* ((open (position #\( word))
         (end (1- (length word)))
         (spec (and open
                    (char= (char word end) #\))
                    (subseq word (1+ open) end)))
         (spec-length (length spec))
         (index 0)
         (entry nil)
         (exit nil))
    (flet ((marker-at-p (position)
             (and (< position spec-length) (find (char spec position) "!@")))
           (mode (position)
             (if (char= (char spec position) #\!) :timestamp :note)))
      (when spec
        (when (and (< index spec-length) (not (find (char spec index) "!@/")))
          (incf index))
        (when (marker-at-p index)
          (setf entry (mode index))
          (incf index))
        (when (and (< index spec-length)
                   (char= (char spec index) #\/)
                   (marker-at-p (1+ index)))
          (setf exit (mode (1+ index)))
          (incf index 2))
        (when (and (= index spec-length) (or entry exit))
          (list (subseq word 0 open) entry exit))))))

(defun edit-todo-logging (document heading)
  "Return HEADING's own or inherited LOGGING property as a plist of :LOG-DONE,
:LOG-REPEAT and :STATES, or NIL when there is none. As in Org's
org-local-logging, LOGGING turns off every logging setting it does not name."
  (multiple-value-bind (value source)
      (effective-property document heading "LOGGING" :inherit t)
    (when source
      (let ((log-done nil)
            (log-repeat nil)
            (states nil))
        (dolist (word (reader-heading-words (substitute #\Space #\Newline value)))
          (let ((option (org-startup-option word)))
            (cond
              ((null option)
               (let ((state (edit-todo-logging-state word)))
                 (when state
                   (push state states))))
              ((eq (second option) :log-done)
               (setf log-done (edit-todo-normalize-mode (third option) heading)))
              ((eq (second option) :log-repeat)
               (setf log-repeat
                     (edit-todo-normalize-mode (third option) heading))))))
        (list :log-done log-done :log-repeat log-repeat :states states)))))

(defun edit-todo-mode-setting (document heading logging options key)
  (cond
    (logging (getf logging key))
    (options
     (edit-todo-normalize-mode
      (if (eq key :log-done)
          (org-workflow-options-log-done options)
          (org-workflow-options-log-repeat options))
      heading))
    (t (edit-todo-normalize-mode (getf (startup-options document) key)
                                 heading))))

(defun edit-todo-logging-state-mode (states previous-state new-state)
  (or (and new-state
           (second (assoc new-state states :test #'string=)))
      (and previous-state
           (third (assoc previous-state states :test #'string=)))))

(defun edit-todo-drawer-setting (document heading options
                                log-drawer log-drawer-supplied-p)
  (let* ((value (if log-drawer-supplied-p
                    log-drawer
                    (edit-todo-property-value
                     document heading options :log-into-drawer
                     "LOG_INTO_DRAWER" "LOGBOOK")))
         (name (cond ((null value) nil)
                     ((or (eq value t)
                          (and (stringp value) (string-equal value "t")))
                      "LOGBOOK")
                     ((and (stringp value) (string-equal value "nil")) nil)
                     ((stringp value) (edit-clock-validated-drawer-name value))
                     (t (edit-todo-error heading :invalid-value
                                         "Invalid TODO log drawer")))))
    name))

(defun edit-todo-selected-sequence (document heading state target)
  (let ((sequences (edit-todo-active-sequences document target))
        (current (node-property heading :todo-keyword)))
    (cond
      ((and current state)
       (let ((sequence (edit-todo-sequence-for-state sequences current target)))
         (unless (edit-todo-sequence-state sequence state)
           (edit-todo-error target :invalid-value
                            "TODO state is outside the heading's sequence"))
         sequence))
      (state (edit-todo-sequence-for-state sequences state target))
      (current (edit-todo-sequence-for-state sequences current target)))))

(defun edit-todo-repeat-state (document heading options sequence target)
  (let* ((value (edit-todo-property-value
                 document heading options :repeat-to-state "REPEAT_TO_STATE"
                 (getf (first (getf sequence :todo-keywords)) :keyword)))
         (state (or (and (stringp value)
                         (unless (string-equal value "nil") value))
                    (and (symbolp value)
                         (not (string-equal (symbol-name value) "NIL"))
                         (string-upcase (symbol-name value)))
                    (getf (first (getf sequence :todo-keywords)) :keyword))))
    (unless (and state
                 (find state (getf sequence :todo-keywords)
                       :key (lambda (entry) (getf entry :keyword))
                       :test #'string=))
      (edit-todo-error target :invalid-value
                       "Repeat state is not a TODO keyword in the selected sequence"))
    state))

(defun edit-todo-own-timestamp-nodes (heading)
  (let ((timestamps nil))
    (labels ((collect (node)
               (when (eq (node-type node) :timestamp)
                 (pushnew node timestamps :test #'eq))
               (dolist (child (node-children node))
                 (collect child))))
      (dolist (title-node (node-property heading :title))
        (when (node-p title-node)
          (collect title-node)))
      (dolist (kind '(:closed :deadline :scheduled))
        (let ((timestamp (node-property heading kind)))
          (when (node-p timestamp)
            (collect timestamp))))
      (let ((section (edit-clock-section heading)))
        (when section (collect section))))
    (stable-sort (nreverse timestamps) #'< :key #'node-begin)))

(defun edit-todo-repeat-timestamp (node at time-context document)
  (let* ((view (timestamp-view node))
         (amount (org-timestamp-view-repeater-amount view))
         (unit (org-timestamp-view-repeater-unit view))
         (mode (org-timestamp-view-repeater-mode view)))
    (when amount
      (multiple-value-bind (start steps)
          (edit-todo-repeat-value
           (org-timestamp-view-start view) at amount unit mode
           time-context document)
        (let ((end (org-timestamp-view-end view)))
          (when end
            (loop repeat steps
                  do (setf end (edit-todo-add-repeater
                                end amount unit time-context))))
          (list :node node :view view :start start :end end
                :text (edit-todo-format-timestamp
                       view start end
                       (and end (not (edit-todo-external-range-p
                                      document node))))))))))

(defun edit-todo-repeat-updates (heading at time-context document)
  (loop for node in (edit-todo-own-timestamp-nodes heading)
        when (or (node-property node :repeater-value)
                 (node-property node :repeater-unit)
                 (node-property node :repeater-type))
          collect (edit-todo-repeat-timestamp node at time-context document)))

(defun edit-todo-canonical-fragment-node (source type &optional drawer-name)
  (edit-clock-canonical-fragment-node source type drawer-name))

(defun edit-todo-canonical-set-planning (expected document heading kind value)
  (let* ((section (edit-properties-headline-section heading))
         (planning (edit-properties-section-planning section))
         (heading-path (edit-node-path document heading)))
    (edit-canonical-set-property expected heading-path kind value)
    (cond
      (planning
       (edit-canonical-set-property expected
                                    (edit-node-path document planning)
                                    kind value))
      (section
       (let* ((section-index (edit-properties-index section
                                                     (node-children heading)))
              (section-path (append heading-path (list section-index))))
         (edit-properties-canonical-insert-child
          expected section-path 0 (edit-properties-canonical-planning kind value))))
      (t
       (edit-properties-canonical-insert-child
        expected heading-path 0
        (list :type :section :post-blank 0 :properties nil
              :children (list (edit-properties-canonical-planning kind value))))))
    expected))

(defun edit-todo-canonical-remove-planning
    (expected document heading kind remove-node-p)
  (let* ((section (edit-properties-headline-section heading))
         (planning (edit-properties-section-planning section))
         (heading-path (edit-node-path document heading))
         (planning-path (and planning (edit-node-path document planning))))
    (edit-canonical-set-property expected heading-path kind nil)
    (edit-canonical-set-property expected planning-path kind nil)
    (when remove-node-p
      (let* ((canonical-planning
               (edit-canonical-node-at-path expected planning-path))
             (properties (getf canonical-planning :properties)))
        (when (notany (lambda (candidate) (getf properties candidate))
                      '(:closed :deadline :scheduled))
          (let* ((section-index (edit-properties-index section
                                                       (node-children heading)))
                 (section-path (append heading-path (list section-index)))
                 (planning-index (edit-properties-index planning
                                                        (node-children section))))
            (edit-properties-canonical-remove-child
             expected section-path planning-index)
            (when (null (getf (edit-canonical-node-at-path expected section-path)
                             :children))
              (edit-properties-canonical-remove-child
               expected heading-path section-index))))))
    expected))

(defun edit-todo-property-patches (document heading key value target)
  (multiple-value-bind (section drawer property)
      (edit-properties-property-target heading key target)
    (let ((source (document-text document)))
      (cond
        (property
         (multiple-value-bind (start end)
             (edit-properties-value-range document property :property)
           (values (list (edit-make-patch start end value target))
                   :replace property section drawer)))
        (drawer
         (let ((close-start (edit-properties-drawer-close-start source drawer)))
           (unless close-start
             (edit-todo-error target :uneditable-structure
                              "Property drawer has no closing marker"))
           (values (list (edit-make-patch
                          close-start close-start
                          (concatenate 'string
                                       (format nil ":~A: ~A" key value)
                                       (edit-properties-line-ending
                                        source close-start))
                          target))
                   :append property section drawer)))
        (t
         (let* ((planning (edit-properties-section-planning section))
                (heading-start (or (and planning (node-begin planning))
                                   (node-post-affiliated heading)))
                (offset (edit-properties-line-after source heading-start)))
           (values
            (list (edit-make-patch
                   offset offset
                   (edit-properties-insertion-text
                    document offset
                    (list ":PROPERTIES:" (format nil ":~A: ~A" key value)
                          ":END:"))
                   target))
            :new property section nil)))))))

(defun edit-todo-canonical-property-change
    (expected document heading key value mode section drawer property target)
  (declare (ignore target))
  (let* ((heading-path (edit-node-path document heading))
         (semantic-value (edit-properties-semantic-value value)))
    (ecase mode
      (:replace
       (edit-canonical-set-property expected
                                    (edit-node-path document property)
                                    :value semantic-value))
      (:append
       (let* ((section-path
                (append heading-path
                        (list (edit-properties-index section
                                                     (node-children heading)))))
              (drawer-path
                (append section-path
                        (list (edit-properties-index drawer
                                                     (node-children section)))))
              (drawer-canonical (edit-canonical-node-at-path expected drawer-path)))
         (setf (getf drawer-canonical :children)
               (append (getf drawer-canonical :children)
                       (list (edit-properties-canonical-property key semantic-value))))))
      (:new
       (let* ((planning (edit-properties-section-planning section))
              (offset (if planning
                          (edit-properties-line-after
                           (document-text document) (node-begin planning))
                          (edit-properties-line-after
                           (document-text document)
                           (node-post-affiliated heading))))
              (drawer-canonical
                (edit-properties-canonical-drawer
                 key semantic-value
                 (edit-properties-blank-lines-after
                  (document-text document) offset))))
         (edit-todo-canonical-insert-section-items
          expected document heading section (list drawer-canonical)))))
    expected))

(defun edit-todo-log-insertion (document heading drawer-name lines target)
  (let* ((section (edit-clock-section heading))
         (existing (and drawer-name (edit-clock-drawer heading drawer-name))))
    (cond
      (existing
       (let* ((offset (edit-clock-drawer-clock-offset document existing))
              (newline (edit-clock-newline document)))
         (values
          (list (edit-make-patch offset offset
                                 (concatenate 'string
                                              (edit-properties-join-lines lines newline)
                                              newline)
                                 target))
          :prepend existing section t)))
      (t
       (let* ((offset (edit-clock-drawer-insertion-offset document heading))
              (newline (edit-clock-newline document))
              (source (document-text document))
              (at-eof-p (= offset (length source)))
              (leading-newline-p
                (and at-eof-p (plusp offset)
                     (not (char= (char source (1- offset)) #\Newline))))
              (preserve-final-newline-p
                (and at-eof-p (document-final-newline-p document)))
              (body (edit-properties-join-lines lines newline))
              (fragment
                (if drawer-name
                    (format nil ":~A:~A~A~A:END:" drawer-name newline body newline)
                    body))
              (replacement
                (concatenate 'string
                             (if leading-newline-p newline "")
                             fragment
                             (if (or (not at-eof-p) preserve-final-newline-p)
                                 newline ""))))
         (values (list (edit-make-patch offset offset replacement target))
                 (if drawer-name :new-drawer :inline) existing section
                 (or (not at-eof-p) preserve-final-newline-p)))))))

(defun edit-todo-canonical-log-fragment (lines drawer-name
                                         &optional (final-newline-p t))
  (let* ((newline (string #\Newline))
         (body (edit-properties-join-lines lines newline))
         (source (if drawer-name
                     (format nil "* Temporary~%:~A:~%~A~%:END:~A"
                             drawer-name body
                             (if final-newline-p newline ""))
                     (format nil "* Temporary~%~A~A"
                             body (if final-newline-p newline ""))))
         (document (parse-org source))
         (heading (find :headline (node-children (document-root document))
                        :key #'node-type))
         (section (edit-clock-section heading)))
    (if drawer-name
        (edit-canonical-node (edit-clock-drawer heading drawer-name))
        (edit-canonical-node section))))

(defun edit-todo-insert-canonical-log
    (expected document heading mode existing section fragment target)
  (declare (ignore target))
  (ecase mode
    (:prepend
     (let* ((section-path (edit-todo-canonical-section-path
                           expected document heading))
            (section-canonical
              (and section-path
                   (edit-canonical-node-at-path expected section-path)))
            (drawer-name (node-property existing :drawer-name))
            (drawer-index
              (and section-canonical
                   (position-if
                    (lambda (child)
                      (and (eq (getf child :type) :drawer)
                           (string-equal
                            (getf (getf child :properties) :drawer-name)
                            drawer-name)))
                    (getf section-canonical :children)))))
       (let* ((path (append section-path (list drawer-index)))
              (canonical (edit-canonical-node-at-path expected path)))
       (setf (getf canonical :children)
             (append (getf fragment :children)
                     (getf canonical :children))))))
    (:new-drawer
     (edit-todo-canonical-insert-section-items
      expected document heading section (list fragment)))
    (:inline
     (let* ((section-path (edit-todo-canonical-section-path
                           expected document heading))
            (section-canonical
              (and section-path
                   (edit-canonical-node-at-path expected section-path)))
            (children (getf section-canonical :children))
            (index (or (position-if-not
                        (lambda (child)
                          (or (member (getf child :type)
                                      '(:planning :property-drawer))
                              (and (eq (getf child :type) :drawer)
                                   (string-equal
                                    (getf (getf child :properties) :drawer-name)
                                    "PROPERTIES"))))
                        children)
                       (length children)))
            (source (document-text document))
            (following
              (and section
                   (find-if-not #'edit-clock-metadata-p
                                (node-children section))))
            (indented-paragraph-p
              (and following
                   (eq (node-type following) :paragraph)
                   (< (node-begin following) (length source))
                   (reader-heading-horizontal-space-p
                    (char source (node-begin following))))))
       (if indented-paragraph-p
           (let* ((items (edit-canonical-copy (getf fragment :children)))
                  (list-node (first items))
                  (item (and list-node (first (getf list-node :children))))
                  (log-paragraph
                    (and item
                         (find :paragraph (getf item :children)
                               :key (lambda (child) (getf child :type)))))
                  (body-paragraph (nth index children)))
             (setf (getf log-paragraph :children)
                   (edit-todo-merge-canonical-inline-children
                    (getf log-paragraph :children)
                    (getf body-paragraph :children)))
             (edit-canonical-set-children
              expected section-path
              (append (subseq children 0 index)
                      items
                      (nthcdr (1+ index) children))))
           (edit-todo-canonical-insert-section-items
            expected document heading section (getf fragment :children))))))
  expected)

(defun edit-todo-merge-canonical-inline-children (left right)
  (loop with merged = nil
        for child in (append left right)
        unless (and merged
                    (eq (getf (first merged) :type) :plain-text)
                    (eq (getf child :type) :plain-text))
          do (push child merged)
        finally (return (nreverse merged))))

(defun edit-todo-node-at-path (document path)
  (let ((node (document-root document)))
    (dolist (index path node)
      (setf node (nth index (node-children node))))))

(defun edit-todo-cookie-original-node (document start end)
  (let ((found nil))
    (labels ((consider (node)
               (when (and (eq (node-type node) :statistics-cookie)
                          (= (node-begin node) start)
                          (= (- (node-end node) (node-post-blank node)) end))
                 (setf found node))))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :statistics-cookie)
         (consider node))
       (when (member (node-type node) '(:headline :inlinetask))
         (dolist (title-node (statistics-cookie-title-nodes node))
           (consider title-node))))
     (document-root document))
    found)))

;; Cookie spans refer to staged source; map only ranges outside replacements.
(defun edit-todo-staged-offset-to-original (offset patches side)
  (let ((delta 0))
    (dolist (patch patches)
      (let* ((start (edit-patch-start patch))
             (end (edit-patch-end patch))
             (new-start (+ start delta))
             (new-end (+ new-start (length (edit-patch-replacement patch))))
             (patch-delta (- (length (edit-patch-replacement patch))
                             (- end start))))
        (cond
          ((< offset new-start)
           (return-from edit-todo-staged-offset-to-original
             (values (- offset delta) t)))
          ((eq side :start)
           (when (< offset new-end)
             (return-from edit-todo-staged-offset-to-original
               (values nil nil)))
           (incf delta patch-delta))
          ((<= offset new-start)
           (return-from edit-todo-staged-offset-to-original
             (values (- offset delta) t)))
          ((< offset new-end)
           (return-from edit-todo-staged-offset-to-original
             (values nil nil)))
          (t
           (incf delta patch-delta)))))
    (values (- offset delta) t)))

(defun edit-todo-staged-range-to-original (start end patches)
  (multiple-value-bind (original-start start-mapped-p)
      (edit-todo-staged-offset-to-original start patches :start)
    (multiple-value-bind (original-end end-mapped-p)
        (edit-todo-staged-offset-to-original end patches :end)
      (when (and start-mapped-p end-mapped-p
                 (<= original-start original-end))
        (list original-start original-end)))))

(defun edit-todo-ancestor-paths (document heading)
  (loop for node = heading then (node-parent node)
        while (and node (not (eq (node-type node) :org-data)))
        collect (edit-node-path document node)))

(defun edit-todo-stage-cookie-updates (document heading base-patches
                                      base-expected target)
  (let* ((source (edit-splice-source (document-text document) base-patches))
         (staged (edit-reparse-source document source target))
         (changes (copy-list base-patches))
         (cookie-patches nil)
         (canonical-updates nil)
         (paths (edit-todo-ancestor-paths document heading)))
    (dolist (path paths)
      (let ((owner (edit-todo-node-at-path staged path)))
        (multiple-value-bind (result updates)
            (update-statistics-cookies staged owner)
          (when updates
            (let ((new-patches nil)
                  (staged-patches nil)
                  (staged-updates nil))
              (loop for span in (edit-result-spans result)
                    for update in updates
                    do (let* ((start (edit-span-start span))
                              (end (edit-span-end span))
                              (original-range
                                (edit-todo-staged-range-to-original
                                 start end changes))
                              (original-node
                                (and original-range
                                     (edit-todo-cookie-original-node
                                      document (first original-range)
                                      (second original-range)))))
                         (if original-node
                             (let* ((new-value
                                      (statistics-cookie-update-new-value update))
                                    (stage-node
                                      (statistics-cookie-update-node update))
                                    (staged-patch
                                      (edit-make-patch start end new-value target))
                                    (patch
                                     (edit-make-patch
                                      (first original-range)
                                      (second original-range)
                                      new-value target)))
                               (push staged-patch staged-patches)
                               (push patch new-patches)
                               (push patch cookie-patches)
                               (push (make-statistics-cookie-update
                                      :node stage-node
                                      :old-value
                                      (statistics-cookie-update-old-value update)
                                      :new-value new-value)
                                     staged-updates)
                               (push (make-statistics-cookie-update
                                      :node original-node
                                      :old-value
                                      (statistics-cookie-update-old-value update)
                                      :new-value new-value)
                                     canonical-updates))
                             nil)))
              (when new-patches
                (setf changes
                      (edit-normalize-patches
                       (append changes new-patches)))
                (let* ((updates (nreverse staged-updates))
                       (expected (edit-canonical-document staged))
                       (title-cookies (make-hash-table :test #'eq)))
                  (statistics-cookie-update-canonical-values
                   staged expected updates)
                  (dolist (node (statistics-cookie-title-nodes owner))
                    (setf (gethash node title-cookies) t))
                  (statistics-cookie-update-title-raw-value
                   staged expected owner title-cookies updates)
                  (setf staged
                        (edit-result-updated-document
                         (edit-apply-patches-result
                          staged (edit-normalize-patches staged-patches)
                          :target target :expected expected))))))))))
    (when canonical-updates
      (let ((updates (nreverse canonical-updates)))
        (statistics-cookie-update-canonical-values
         document base-expected updates)
        (dolist (path paths)
          (let* ((owner (edit-todo-node-at-path document path))
                 (staged-owner (edit-todo-node-at-path staged path)))
            ;; Keep title edits already present in STAGED when cookies change.
            (edit-canonical-set-property
             base-expected (edit-node-path document owner) :raw-value
             (node-property staged-owner :raw-value))))))
    (values (nreverse cookie-patches) base-expected)))

(defun edit-todo-canonical-timestamp-updates
    (expected document heading updates target)
  (declare (ignore target))
  (let ((index (make-hash-table :test #'eq))
        (title-patches nil)
        (planning (edit-properties-section-planning
                   (edit-properties-headline-section heading))))
    (statistics-cookie-index-canonical-nodes
     (document-root document) expected index)
    (dolist (update updates)
      (let* ((node (getf update :node))
             (replacement
               (edit-todo-canonical-fragment-node
                (format nil "~A~%" (getf update :text)) :timestamp))
             (canonical (gethash node index))
             (planning-kind
               (and planning
                    (find-if (lambda (kind)
                              (let ((planning-node
                                      (node-property planning kind)))
                                (and (node-p planning-node)
                                     (= (node-begin node)
                                        (node-begin planning-node))
                                     (= (node-end node)
                                        (node-end planning-node)))))
                             '(:scheduled :deadline :closed)))))
        (setf (getf replacement :post-blank) (node-post-blank node))
        (if planning-kind
            (let ((planning-path (edit-node-path document planning))
                  (heading-path (edit-node-path document heading)))
              (edit-canonical-set-property expected planning-path
                                           planning-kind replacement)
              (edit-canonical-set-property expected heading-path
                                           planning-kind replacement))
            (setf (getf canonical :type) (getf replacement :type)
                  (getf canonical :post-blank) (getf replacement :post-blank)
                  (getf canonical :properties) (getf replacement :properties)
                  (getf canonical :children) (getf replacement :children)))
        (when (member node (statistics-cookie-title-nodes heading) :test #'eq)
          (multiple-value-bind (title-start title-end)
              (edit-headline-title-span document heading)
            (declare (ignore title-end))
            (push (edit-make-patch
                   (- (node-begin node) title-start)
                   ;; Preserve any separating whitespace after the timestamp.
                   (- (- (node-end node) (node-post-blank node)) title-start)
                   (getf update :text))
                  title-patches)))))
    (when title-patches
      (let* ((raw-value (node-property heading :raw-value))
             (new-value (edit-splice-source raw-value
                                            (edit-normalize-patches title-patches))))
        (edit-canonical-set-property expected
                                     (edit-node-path document heading)
                                     :raw-value new-value)))
    expected))

(defun edit-todo-canonical-section-path (expected document heading)
  (let* ((heading-path (edit-node-path document heading))
         (canonical (edit-canonical-node-at-path expected heading-path))
         (index (position :section (getf canonical :children)
                         :key (lambda (child) (getf child :type)))))
    (and index (append heading-path (list index)))))

(defun edit-todo-canonical-insert-section-items
    (expected document heading original-section items)
  (declare (ignore original-section))
  (let* ((heading-path (edit-node-path document heading))
         (section-path (edit-todo-canonical-section-path
                        expected document heading)))
    (unless section-path
      (edit-properties-canonical-insert-child
       expected heading-path 0
       (list :type :section :post-blank 0 :properties nil :children nil))
      (setf section-path (append heading-path '(0))))
    (let* ((canonical (edit-canonical-node-at-path expected section-path))
           (children (getf canonical :children))
           (index (or (position-if-not
                       (lambda (child)
                         (or (member (getf child :type)
                                     '(:planning :property-drawer))
                             (and (eq (getf child :type) :drawer)
                                  (string-equal
                                   (getf (getf child :properties) :drawer-name)
                                   "PROPERTIES"))))
                       children)
                      (length children))))
      (edit-canonical-set-children
       expected section-path
       (append (subseq children 0 (min index (length children)))
               items
               (nthcdr (min index (length children)) children)))
      expected)))

(defun edit-todo-repeat-p (updates)
  (not (null updates)))

(defun edit-todo-done-state-p (sequence state)
  (and state
       (member state (getf sequence :done-keywords)
               :key (lambda (entry) (getf entry :keyword))
               :test #'string=)))

(defun edit-todo-build-log-lines
    (previous-state new-state timestamp mode note target)
  (when (eq mode :note)
    (unless (stringp note)
      (edit-todo-error target :invalid-value
                       "TODO note logging requires a note string")))
  (when mode
    (let* ((new-field (if new-state (format nil "~S" new-state) ""))
           (old-field (if previous-state (format nil "~S" previous-state) ""))
           (prefix (concatenate 'string "- State " new-field))
           (from-column 21)
           (time-column 39)
           (header
             (concatenate
              'string prefix
              (make-string (max 1 (- from-column (length prefix)))
                           :initial-element #\Space)
              "from " old-field
              (make-string
               (max 1 (- time-column
                         (+ (length prefix)
                            (max 1 (- from-column (length prefix)))
                            (length "from ")
                            (length old-field))))
               :initial-element #\Space)
              timestamp
              (if (eq mode :note) " \\\\" ""))))
      (append (list header)
              (when (eq mode :note)
                (edit-todo-indented-note-lines note))))))

(defun edit-todo-indented-note-lines (note)
  (let ((start 0)
        (lines nil))
    (loop
      for end = (position #\Newline note :start start)
      do (let ((line (string-right-trim '(#\Return)
                                        (subseq note start end))))
           (push (if (zerop (length line))
                     ""
                     (concatenate 'string "  " line))
                 lines))
      unless end return (nreverse lines)
      do (setf start (1+ end)))))

(defun edit-todo-build-closing-note-lines (timestamp note target)
  (unless (stringp note)
    (edit-todo-error target :invalid-value
                     "TODO note logging requires a note string"))
  (cons (format nil "- CLOSING NOTE ~A \\\\" timestamp)
        (edit-todo-indented-note-lines note)))

(defun edit-todo-final-planning-present-p
    (planning changes kind)
  (let ((change (assoc kind changes)))
    (cond ((and change (eq (getf (cdr change) :action) :set)) t)
          ((and change (eq (getf (cdr change) :action) :remove)) nil)
          (t (and planning (node-property planning kind))))))

(defun edit-todo-planning-patches (document heading changes target)
  (let* ((source (document-text document))
         (section (edit-properties-headline-section heading))
         (planning (edit-properties-section-planning section))
         (patches nil))
    (dolist (change changes)
      (let* ((kind (car change))
             (data (cdr change))
             (action (getf data :action))
             (new-value (getf data :value))
             (old-value (and planning (node-property planning kind))))
        (ecase action
          (:set
           (setf (getf (cdr change) :canonical-value)
                 (edit-properties-planning-value document kind new-value target))
           (setf patches
                 (append patches
                         (cond
                           (old-value
                            (list (edit-make-patch
                                   (node-begin old-value) (node-end old-value)
                                   new-value target)))
                           (planning
                            (multiple-value-bind (line-end after-line)
                                (edit-properties-line-bounds source
                                                             (node-begin planning))
                              (declare (ignore after-line))
                              (list (edit-make-patch
                                     line-end line-end
                                     (format nil " ~A: ~A"
                                             (edit-properties-planning-label kind)
                                             new-value)
                                     target))))
                           (t
                            (let* ((offset
                                     (edit-properties-line-after
                                      source (node-post-affiliated heading))))
                              (list (edit-make-patch
                                     offset offset
                                     (edit-properties-insertion-text
                                      document offset
                                      (list (format nil "~A: ~A"
                                                    (edit-properties-planning-label kind)
                                                    new-value)))
                                     target))))))))
          (:remove
           (when old-value
             (let ((other-p
                     (some (lambda (other)
                             (and (not (eq other kind))
                                  (edit-todo-final-planning-present-p
                                   planning changes other)))
                           '(:closed :deadline :scheduled))))
               (multiple-value-bind (start end)
                   (edit-properties-planning-removal-range
                    source planning
                    (cons (node-begin old-value) (node-end old-value))
                    kind (not other-p))
                 (push (edit-make-patch start end "" target) patches))))))))
    (values
     (remove-duplicates
      (nreverse patches)
      :test (lambda (left right)
              (and (= (edit-patch-start left) (edit-patch-start right))
                   (= (edit-patch-end left) (edit-patch-end right))
                   (string= (edit-patch-replacement left)
                            (edit-patch-replacement right)))))
     planning section)))

(defun edit-todo-canonical-close-property-drawer
    (expected document heading timestamp)
  (let* ((section (edit-properties-headline-section heading))
         (planning (edit-properties-section-planning section))
         (drawer (and (null planning)
                      section
                      (find :property-drawer (node-children section)
                            :key #'node-type))))
    (when drawer
      (let* ((source (document-text document))
             (drawer-source (subseq source (node-begin drawer) (node-end drawer)))
             (fragment-document
               (parse-org
                (format nil "* Temporary~%CLOSED: ~A~%~A"
                        timestamp drawer-source)
                :limits (document-limits document)))
             (fragment-heading
               (find :headline
                     (node-children (document-root fragment-document))
                     :key #'node-type))
             (fragment-section (edit-properties-headline-section
                                fragment-heading))
             (fragment-drawer
               (and fragment-section
                    (find-if
                     (lambda (candidate)
                       (and (eq (node-type candidate) :drawer)
                            (string-equal
                             (node-property candidate :drawer-name)
                             "PROPERTIES")))
                     (node-children fragment-section))))
             (section-path (edit-node-path document section))
             (canonical-section
               (edit-canonical-node-at-path expected section-path))
             (children (getf canonical-section :children))
             (drawer-index
               (position :property-drawer children
                         :key (lambda (child) (getf child :type)))))
        (setf (nth drawer-index children) (edit-canonical-node fragment-drawer)
              (getf canonical-section :children) children)))
    expected))

(defun edit-todo-canonical-planning-changes
    (expected document heading changes target)
  (declare (ignore target))
  (dolist (change changes expected)
    (let* ((kind (car change))
           (data (cdr change))
           (action (getf data :action)))
      (ecase action
        (:set
         (edit-todo-canonical-set-planning
          expected document heading kind (getf data :canonical-value))
         (when (eq kind :closed)
           (edit-todo-canonical-close-property-drawer
            expected document heading (getf data :value))))
        (:remove
         (let* ((section (edit-properties-headline-section heading))
                (planning (edit-properties-section-planning section))
                (remove-node-p
                  (notany (lambda (other)
                            (and (not (eq other kind))
                                 (edit-todo-final-planning-present-p
                                  planning changes other)))
                          '(:closed :deadline :scheduled))))
           (edit-todo-canonical-remove-planning
            expected document heading kind remove-node-p)))))))

(defun edit-todo-event-timestamp (at time-context)
  (let ((value (if (org-time-value-offset-minutes at)
                   (edit-todo-localize-time-value at time-context)
                   at)))
    (format nil "[~A]" (edit-todo-format-point-timestamp value))))

(defun edit-todo-coalesce-insertions (patches target)
  (let ((groups (make-hash-table :test #'eql))
        (offsets nil)
        (replacements nil))
    (dolist (patch patches)
      (let ((start (edit-patch-start patch))
            (end (edit-patch-end patch))
            (replacement (edit-patch-replacement patch)))
        (if (= start end)
            (multiple-value-bind (existing present-p) (gethash start groups)
              (if present-p
                  (setf (gethash start groups)
                        (concatenate 'string existing replacement))
                  (progn
                    (push start offsets)
                    (setf (gethash start groups) replacement))))
            (push patch replacements))))
    (edit-normalize-patches
     (append (nreverse replacements)
             (mapcar (lambda (offset)
                       (edit-make-patch offset offset
                                        (gethash offset groups) target))
                     (nreverse offsets))))))

(defun edit-todo-source-diff-patch
    (before after target &key trim-empty-state-space-p)
  (let ((old (document-text before))
        (new (document-text after)))
    (let ((prefix (loop for index below (min (length old) (length new))
                        while (char= (char old index) (char new index))
                        finally (return index))))
      (unless (string= old new)
        (let ((old-end (length old))
              (new-end (length new)))
          (loop while (and (> old-end prefix)
                           (> new-end prefix)
                           (char= (char old (1- old-end))
                                  (char new (1- new-end))))
                 do (decf old-end) (decf new-end))
          (when (and trim-empty-state-space-p
                     (= new-end prefix)
                     (< old-end (length old))
                     (reader-heading-horizontal-space-p (char old old-end)))
            (incf old-end))
          (list (edit-make-patch prefix old-end
                                 (subseq new prefix new-end) target)))))))

(defun edit-todo-repeat-source-patches (updates target)
  (mapcar (lambda (update)
            (let ((node (getf update :node)))
              (edit-make-patch
               (node-begin node)
               (- (node-end node) (node-post-blank node))
               (getf update :text)
               target)))
          updates))

(defun edit-todo-local-planning-value (document heading kind)
  (let* ((planning (edit-properties-section-planning
                    (edit-properties-headline-section heading)))
         (node (and planning (node-property planning kind))))
    (when node
      (subseq (document-text document)
              (node-begin node)
              (- (node-end node) (node-post-blank node))))))

(defun edit-todo-marker-log-mode (sequence previous-state new-state)
  (let* ((new-entry (and sequence new-state
                         (edit-todo-sequence-state sequence new-state)))
         (old-entry (and sequence previous-state
                         (edit-todo-sequence-state sequence previous-state)))
         (entry-mode (and new-entry (getf new-entry :logging-mode))))
    (cond ((and entry-mode (not (eq entry-mode :none))) entry-mode)
          ((and old-entry (getf old-entry :exit-logging-p)) :timestamp)
          (t nil))))

(defun transition-heading-todo
    (document target state &key (at (values nil) at-supplied-p) time-context
                               workflow-options note
                               (log-drawer (values nil) log-drawer-supplied-p))
  "Transition a headline to STATE and return its edit and transition results.
AT is required for every transition; logging and repeat behavior use the
document's normalized TODO sequence and the supplied workflow settings."
  (let* ((heading (edit-headline-resolve-target document target))
         (previous-state (node-property heading :todo-keyword)))
    (unless at-supplied-p
      (edit-todo-error heading :invalid-value
                       "TODO transitions require an explicit :AT value"))
    (unless (or (null state) (stringp state))
      (edit-todo-error heading :invalid-value
                       "TODO state must be a configured string or NIL"))
    (unless (or (null workflow-options)
                (org-workflow-options-p workflow-options))
      (edit-todo-error heading :invalid-value
                       "WORKFLOW-OPTIONS must be an Org workflow options value"))
    (let* ((at-value (edit-todo-normalize-at at))
           (sequence (and (or previous-state state)
                          (edit-todo-selected-sequence
                           document heading state heading))))
      (let* ((requested-done-p (and sequence
                                    (edit-todo-done-state-p sequence state)))
             (repeat-updates (if requested-done-p
                                 (edit-todo-repeat-updates
                                  heading at-value time-context document)
                                 nil))
             (repeating-p (edit-todo-repeat-p repeat-updates))
             (new-state (if repeating-p
                            (edit-todo-repeat-state
                             document heading workflow-options sequence heading)
                            state))
             (new-done-p (and sequence
                              (edit-todo-done-state-p sequence new-state)))
             (previous-done-p (and sequence
                                   (edit-todo-done-state-p sequence previous-state)))
             (state-changed-p (not (equal previous-state new-state)))
             (logging (edit-todo-logging document heading))
             (done-mode (edit-todo-mode-setting
                         document heading logging workflow-options :log-done))
             (repeat-mode (edit-todo-mode-setting
                           document heading logging workflow-options
                           :log-repeat))
             (marker-mode (if logging
                              (edit-todo-logging-state-mode
                               (getf logging :states) previous-state
                               (if repeating-p state new-state))
                              (edit-todo-marker-log-mode
                               sequence previous-state
                               (if repeating-p state new-state))))
             (state-log-mode (if repeating-p repeat-mode marker-mode))
             (closed-value (edit-todo-local-planning-value
                            document heading :closed))
             (closed-timestamp nil)
             (planning-changes nil)
             (log-lines nil)
             (timestamp (edit-todo-event-timestamp at-value time-context))
             (log-drawer-name
               (edit-todo-drawer-setting
                document heading workflow-options
                log-drawer log-drawer-supplied-p)))
        (when (and state-changed-p new-done-p (not repeating-p) done-mode)
          (push (cons :closed (list :action :set :value timestamp))
                planning-changes)
          (setf closed-timestamp timestamp))
        (when (and closed-value previous-done-p (not new-done-p))
          (push (cons :closed (list :action :remove)) planning-changes)
          (setf closed-timestamp nil))
        (when (and (or repeating-p
                       (and state-changed-p new-done-p))
                   (let* ((planning
                            (edit-properties-section-planning
                             (edit-properties-headline-section heading)))
                          (scheduled (and planning
                                          (node-property planning :scheduled))))
                     (and scheduled
                          (null (node-property scheduled :repeater-value)))))
          (push (cons :scheduled (list :action :remove)) planning-changes))
        (setf planning-changes (nreverse planning-changes))
        (when (and (or state-changed-p repeating-p) state-log-mode)
          (setf log-lines
                (append log-lines
                        (edit-todo-build-log-lines
                         previous-state (if repeating-p state new-state)
                         timestamp state-log-mode note heading))))
        (when (and state-changed-p new-done-p (not repeating-p)
                   (eq done-mode :note))
          (setf log-lines
                (append log-lines
                        (edit-todo-build-closing-note-lines
                         timestamp note heading))))
        (let* ((todo-document (set-heading-todo document heading new-state))
               (todo-patches
                 (edit-todo-source-diff-patch
                  document todo-document heading
                  :trim-empty-state-space-p (null new-state)))
               (expected (edit-canonical-document todo-document))
               (repeat-patches
                 (edit-todo-repeat-source-patches repeat-updates heading))
               (repeat-timestamps (mapcar (lambda (update) (getf update :text))
                                          repeat-updates))
               (base-patches (append todo-patches repeat-patches))
               (last-repeat-p
                 (and repeating-p
                      (or repeat-mode
                          (edit-clock-entry-clock-nodes heading))))
               (property-change-data nil)
               (log-change-data nil))
          (multiple-value-bind (planning-patches planning section)
              (edit-todo-planning-patches
               document heading planning-changes heading)
            (declare (ignore planning section))
            (setf base-patches (append base-patches planning-patches))
            (setf expected
                  (edit-todo-canonical-timestamp-updates
                   expected document heading repeat-updates heading))
            (when last-repeat-p
              (multiple-value-bind (property-patches mode property
                                    property-section drawer)
                  (edit-todo-property-patches
                   document heading "LAST_REPEAT" timestamp heading)
                (setf base-patches (append base-patches property-patches)
                      property-change-data
                      (list mode property property-section drawer))))
            (when log-lines
              (multiple-value-bind (log-patches mode existing log-section
                                    final-newline-p)
                  (edit-todo-log-insertion
                   document heading log-drawer-name log-lines heading)
                (setf base-patches (append base-patches log-patches)
                      log-change-data
                      (list mode existing log-section
                            (edit-todo-canonical-log-fragment
                             log-lines log-drawer-name final-newline-p)))))
            (setf base-patches
                  (edit-todo-coalesce-insertions base-patches heading))
            (multiple-value-bind (cookie-patches final-expected)
                (edit-todo-stage-cookie-updates
                 document heading base-patches expected heading)
              (let ((expected
                      (edit-todo-canonical-planning-changes
                       final-expected document heading planning-changes heading)))
                (when property-change-data
                  (destructuring-bind (mode property property-section drawer)
                      property-change-data
                    (setf expected
                          (edit-todo-canonical-property-change
                           expected document heading "LAST_REPEAT" timestamp
                           mode property-section drawer property heading))))
                (when log-change-data
                  (destructuring-bind (mode existing log-section fragment)
                      log-change-data
                    (setf expected
                          (edit-todo-insert-canonical-log
                           expected document heading mode existing log-section
                           fragment heading))))
                (let* ((patches
                        (edit-todo-coalesce-insertions
                         (append base-patches cookie-patches) heading))
                       (result
                         (edit-apply-patches-result
                          document patches :target heading :expected expected))
                       (closed-remove-p
                         (some (lambda (change)
                                 (and (eq (car change) :closed)
                                      (eq (getf (cdr change) :action) :remove)))
                               planning-changes))
                       (result-closed
                         (if closed-remove-p nil
                             (or closed-timestamp closed-value))))
                  (values
                   result
                   (make-transition-result
                    :previous-state previous-state
                    :new-state new-state
                    :log-events log-lines
                    :rescheduled-timestamps repeat-timestamps
                    :closed-timestamp result-closed)))))))))))

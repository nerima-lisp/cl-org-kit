;;;; src/export-icalendar.lisp
(in-package #:org-kit)

(defparameter +icalendar-option-defaults+
  '((:use-deadline :event-if-not-todo :todo-due) (:use-scheduled :todo-start)
    (:include-todo . nil)
    (:alarm-time . 0)
    (:force-alarm . nil)
    (:exclude-tags)
    (:summary-prefix-scheduled . "S: ")
    (:summary-prefix-deadline . "DL: ")
    (:categories :local-tags :category)
    (:with-timestamps . :active)
    (:todo-unscheduled-start . :recurring-deadline-warning)
    (:include-sexps . t)
    (:include-body . t)
    (:store-uid . nil)
    (:timezone)
    (:date-time-format . ":%Y%m%dT%H%M%S")
    (:ttl)
    (:combined-name . "OrgMode")
    (:description)))

(defparameter +icalendar-keyword-names+
  '("ICALENDAR_USE_DEADLINE" "ICALENDAR_USE_SCHEDULED"
                             "ICALENDAR_INCLUDE_TODO"
                             "ICALENDAR_ALARM_TIME"
                             "ICALENDAR_FORCE_ALARM"
                             "ICALENDAR_EXCLUDE_TAGS"
                             "ICALENDAR_SUMMARY_PREFIX_SCHEDULED"
                             "ICALENDAR_SUMMARY_PREFIX_DEADLINE"
                             "ICALENDAR_CATEGORIES"
                             "ICALENDAR_WITH_TIMESTAMPS"
                             "ICALENDAR_TODO_UNSCHEDULED_START"
                             "ICALENDAR_INCLUDE_SEXPS"
                             "ICALENDAR_INCLUDE_BODY"
                             "ICALENDAR_STORE_UID"
                             "ICALENDAR_TIMEZONE"
                             "ICALENDAR_DATE_TIME_FORMAT"
                             "ICALENDAR_TTL"
                             "ICALENDAR_COMBINED_NAME"
                             "ICALENDAR_DESCRIPTION"
                             "ICAL-TTL"
                             "BIND"))

(defparameter +icalendar-option-keys+
  '(:use-deadline :use-scheduled
                  :include-todo
                  :alarm-time
                  :force-alarm
                  :exclude-tags
                  :summary-prefix-scheduled
                  :summary-prefix-deadline
                  :categories
                  :with-timestamps
                  :todo-unscheduled-start
                  :include-sexps
                  :include-body
                  :store-uid
                  :timezone
                  :date-time-format
                  :ttl
                  :combined-name
                  :description))

(defstruct
    (%icalendar-builder (:constructor %make-icalendar-builder)
                        (:conc-name %icalendar-builder-)
                        (:copier nil))
  (buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
  (characters 0)
  (octets 0)
  (limits nil :read-only t))

(defun %icalendar-minimum-limits (documents name)
  (reduce #'min
          documents
          :key
          (lambda (document)
            (limit-value (document-limits document) name))))

(defun %icalendar-document-limits (documents)
  (if documents
      (make-limits :max-output-length
                   (%icalendar-minimum-limits documents :max-output-length)
                   :max-output-octets
                   (%icalendar-minimum-limits documents :max-output-octets)
                   :max-node-count
                   (%icalendar-minimum-limits documents :max-node-count))
      (make-limits)))

(defun %icalendar-check-documents (documents)
  (unless
      (and (export-proper-list-p documents)
           (every
            (lambda (document)
              (and (document-p document) (document-root document)))
            documents))
    (error 'org-export-error
           :message
           "Expected parsed Org documents"
           :backend
           :icalendar
           :reason
           :invalid-documents))
  (let* ((limits (%icalendar-document-limits documents))
         (nodes
          (loop for document in documents
                sum (let ((count 0))
                      (map-nodes
                       (lambda (node)
                         (declare (ignore node))
                         (incf count))
                       (document-root document))
                      count))))
    (check-limit limits :max-node-count nodes)
    limits))

;; RFC 5545 measures the physical line in octets; Org 9.8.10 counts characters.
(defun %icalendar-utf8-width (character)
  (let ((code (char-code character)))
    (cond
      ((<= code #x7f) 1)
      ((<= code #x7ff) 2)
      ((<= #xd800 code #xdfff)
       (error 'org-export-error
              :message "Invalid Unicode character"
              :backend :icalendar
              :reason :encoding))
      ((<= code #xffff) 3)
      (t 4))))

(defun %icalendar-builder-append-character (builder character)
  (let* ((characters (1+ (%icalendar-builder-characters builder)))
         (octets
          (+ (%icalendar-builder-octets builder)
             (%icalendar-utf8-width character)))
         (limits (%icalendar-builder-limits builder)))
    (check-limit limits :max-output-length characters)
    (check-limit limits :max-output-octets octets)
    (vector-push-extend character (%icalendar-builder-buffer builder))
    (setf (%icalendar-builder-characters builder) characters
          (%icalendar-builder-octets builder) octets))
  builder)

(defun %icalendar-builder-append-text (builder text)
  (loop for character across text
        do (%icalendar-builder-append-character builder character))
  builder)

(defun %icalendar-text-character-width (character escaped-p)
  (if (and escaped-p (find character '(#\\ #\; #\,)))
      (+ 1 (%icalendar-utf8-width character))
      (%icalendar-utf8-width character)))

(defun %icalendar-append-fragment-character (builder character
                                                     escaped-p
                                                     current-line-octets)
  (let ((width (%icalendar-text-character-width character escaped-p)))
    (when (> (+ current-line-octets width) 75)
      (%icalendar-builder-append-text builder
                                      (format nil "~C~C " #\Return #\Newline))
      (setf current-line-octets 1))
    (when (and escaped-p (find character '(#\\ #\; #\,)))
      (%icalendar-builder-append-character builder #\\))
    (%icalendar-builder-append-character builder character)
    (+ current-line-octets width)))

(defun %icalendar-append-escaped-newline (builder current-line-octets)
  (when (> (+ current-line-octets 2) 75)
    (%icalendar-builder-append-text builder
                                    (format nil "~C~C " #\Return #\Newline))
    (setf current-line-octets 1))
  (%icalendar-builder-append-character builder #\\)
  (%icalendar-builder-append-character builder #\n)
  (+ current-line-octets 2))

(defun %icalendar-append-line (builder fragments)
  (let ((line-octets 0))
    (dolist (fragment fragments)
      (destructuring-bind (text . escaped-p) fragment
        (unless (stringp text)
          (error 'type-error :datum text :expected-type 'string))
        (loop for index from 0 below (length text)
              for character = (char text index)
              do (cond
                   ((and escaped-p (char= character #\Return))
                     (setf line-octets (%icalendar-append-escaped-newline
                                        builder
                                        line-octets))
                     (when
                         (and (< (1+ index) (length text))
                              (char= (char text (1+ index)) #\Newline))
                       (incf index)))
                   ((and escaped-p (char= character #\Newline))
                    (setf line-octets (%icalendar-append-escaped-newline builder
                                                                         line-octets)))
                   (t
                    (setf line-octets (%icalendar-append-fragment-character
                                       builder
                                       character
                                       escaped-p
                                       line-octets)))))))
    (%icalendar-builder-append-text builder
                                    (format nil "~C~C" #\Return #\Newline)))
  builder)

(defun %icalendar-line (builder name value &key text-value-p)
  (%icalendar-append-line builder
                          (list (cons name nil)
                                (cons ":" nil)
                                (cons (or value "") text-value-p))))

(defun %icalendar-keyword-option-name (key)
  (when (stringp key)
    (cond
      ((string-equal key "ICAL-TTL") :ttl)
      ((and (>= (length key) 10) (string-equal key "ICALENDAR_" :end1 10))
       (let ((suffix (subseq key 10)))
         (find-if
          (lambda (option)
            (string-equal suffix (substitute #\_ #\- (symbol-name option))))
          +icalendar-option-keys+)))
      (t nil))))

(defun %icalendar-bind-option (value)
  (when (stringp value)
    (let* ((start (position-if #'export-whitespace-p value))
           (name (and start (subseq value 0 start)))
           (raw (and start (string-trim '(#\Space #\Tab) (subseq value start))))
           (prefix "org-icalendar-"))
      (when
          (and name
               raw
               (>= (length name) (length prefix))
               (string-equal name prefix :end1 (length prefix)))
        (let ((key
               (or
                (find-if
                 (lambda (option)
                   (string-equal
                    (substitute #\- #\_ (subseq name (length prefix)))
                    (symbol-name option)))
                 +icalendar-option-keys+)
                (error 'org-export-error
                       :message
                       "Unknown Org iCalendar BIND option"
                       :backend
                       :icalendar
                       :reason
                       :invalid-option))))
          (when (member key +icalendar-option-keys+ :test #'eq)
            (values key
                    (cond
                      ((and (plusp (length raw)) (char= (char raw 0) #\())
                       (let ((end (position #\) raw :from-end t)))
                         (unless end
                           (error 'org-export-error
                                  :message
                                  "Invalid iCalendar BIND value"
                                  :backend
                                  :icalendar
                                  :reason
                                  :invalid-option))
                         (mapcar
                          (lambda (item)
                            (string-trim '(#\" #\Space #\Tab) item))
                          (export-split-words (subseq raw 1 end)))))
                      ((and (plusp (length raw)) (char= (char raw 0) #\"))
                        (unless
                            (and (> (length raw) 1)
                                 (char= (char raw (1- (length raw))) #\"))
                          (error 'org-export-error
                                 :message
                                 "Invalid iCalendar BIND string"
                                 :backend
                                 :icalendar
                                 :reason
                                 :invalid-option))
                        (subseq raw 1 (1- (length raw))))
                      (t raw)))))))))

(defun %icalendar-option-kind (key)
  (cond
    ((eq key :include-todo) :include-todo)
    ((eq key :include-body) :body)
    ((member key '(:force-alarm :include-sexps :store-uid) :test #'eq) :boolean)
    ((eq key :alarm-time) :integer)
    ((member key
             '(:use-deadline :use-scheduled :exclude-tags :categories)
             :test
             #'eq) :list)
    ((member key '(:with-timestamps :todo-unscheduled-start) :test #'eq)
     :symbol)
    (t :string)))

(defun %icalendar-list-option-item (key item)
  (let ((text
         (if (stringp item)
             item
             (if (symbolp item)
                 (symbol-name item)
                 (error 'type-error
                        :datum
                        item
                        :expected-type
                        '(or string symbol))))))
    (cond
      ((member key '(:use-deadline :use-scheduled) :test #'eq)
       (let ((allowed
              (if (eq key :use-deadline)
                  '(:event-if-not-todo :event-if-todo
                                       :event-if-todo-not-done
                                       :todo-due)
                  '(:event-if-not-todo :event-if-todo
                                       :event-if-todo-not-done
                                       :todo-start))))
         (or
          (find-if
           (lambda (selector)
             (string-equal text (symbol-name selector)))
           allowed)
          (error 'org-export-error
                 :message
                 "Unknown iCalendar event selector"
                 :backend
                 :icalendar
                 :reason
                 :invalid-option))))
      ((eq key :categories)
       (let ((category (string-downcase text)))
         (if (member category
                     '("local-tags" "category" "todo-state" "all-tags")
                     :test
                     #'string=)
             category
             (error 'org-export-error
                    :message
                    "Unknown iCalendar category selector"
                    :backend
                    :icalendar
                    :reason
                    :invalid-option))))
      (t (string-downcase text)))))

(defun %icalendar-parse-option-value (key value)
  (case (%icalendar-option-kind key)
    (:boolean
     (cond
       ((or (eq value t) (and (stringp value) (string-equal value "t"))) t)
       ((or (null value) (and (stringp value) (string-equal value "nil"))) nil)
       (t
        (error 'org-export-error
               :message
               "Invalid iCalendar boolean option"
               :backend
               :icalendar
               :reason
               :invalid-option))))
    (:include-todo
     (cond
       ((or (eq value t) (and (stringp value) (string-equal value "t"))) t)
       ((or (null value) (and (stringp value) (string-equal value "nil"))) nil)
       ((member value '(:all :unblocked) :test #'eq) value)
       ((and (stringp value)
             (member (string-downcase value)
                     '("all" "unblocked")
                     :test
                     #'string=))
        (if (string-equal value "all")
            :all
            :unblocked))
       ((stringp value) (export-split-words value))
       ((and (listp value) (export-proper-list-p value))
        (mapcar
         (lambda (item)
           (if (stringp item)
               (copy-seq item)
               (if (symbolp item)
                   (symbol-name item)
                   (error 'type-error
                          :datum
                          item
                          :expected-type
                          '(or string symbol)))))
         value))
       (t
        (error 'org-export-error
               :message
               "Invalid iCalendar TODO option"
               :backend
               :icalendar
               :reason
               :invalid-option))))
    (:body
     (cond
       ((or (eq value t) (and (stringp value) (string-equal value "t"))) t)
       ((or (null value) (and (stringp value) (string-equal value "nil"))) nil)
       ((integerp value)
        (if (minusp value)
            (error 'org-export-error
                   :message
                   "Invalid iCalendar body length"
                   :backend
                   :icalendar
                   :reason
                   :invalid-option)
            value))
       ((stringp value)
        (let ((number (ignore-errors (parse-integer value :junk-allowed nil))))
          (if (and number (not (minusp number)))
              number
              (error 'org-export-error
                     :message
                     "Invalid iCalendar body option"
                     :backend
                     :icalendar
                     :reason
                     :invalid-option))))
       (t
        (error 'org-export-error
               :message
               "Invalid iCalendar body option"
               :backend
               :icalendar
               :reason
               :invalid-option))))
    (:integer
     (let ((number
            (if (integerp value)
                value
                (and (stringp value)
                     (ignore-errors (parse-integer value :junk-allowed nil))))))
       (unless (and (integerp number) (not (minusp number)))
         (error 'org-export-error
                :message
                "Invalid iCalendar integer option"
                :backend
                :icalendar
                :reason
                :invalid-option))
       number))
    (:list
     (cond
       ((null value) nil)
       ((and (listp value) (export-proper-list-p value))
        (mapcar
         (lambda (item)
           (%icalendar-list-option-item key item))
         value))
       ((stringp value)
        (mapcar
         (lambda (item)
           (%icalendar-list-option-item key item))
         (export-split-words value)))
       (t
        (error 'org-export-error
               :message
               "Invalid iCalendar list option"
               :backend
               :icalendar
               :reason
               :invalid-option))))
    (:symbol
     (let ((allowed
            (case key
              (:with-timestamps '(:active :active-exclude-diary :inactive :all))
              (:todo-unscheduled-start
               '(:recurring-deadline-warning :deadline-warning
                                             :current-datetime)))))
       (cond
         ((member value allowed :test #'eq) value)
         ((null value) nil)
         ((and (stringp value) (string-equal value "nil")) nil)
         ((and (eq key :with-timestamps)
               (or (eq value t) (and (stringp value) (string-equal value "t"))))
          :all)
         ((stringp value)
          (or
           (find-if
            (lambda (mode)
              (string-equal value (symbol-name mode)))
            allowed)
           (error 'org-export-error
                  :message
                  "Invalid iCalendar mode option"
                  :backend
                  :icalendar
                  :reason
                  :invalid-option)))
         (t
          (error 'org-export-error
                 :message
                 "Invalid iCalendar mode option"
                 :backend
                 :icalendar
                 :reason
                 :invalid-option)))))
    (:string
      (unless (stringp value)
        (error 'type-error :datum value :expected-type 'string))
      (when (and (eq key :ttl) (some #'export-control-character-p value))
        (error 'org-export-error
               :message
               "Invalid iCalendar TTL"
               :backend
               :icalendar
               :reason
               :invalid-option))
      (copy-seq value))))

(defun %icalendar-explicit-options (options)
  (unless (and (export-proper-list-p options) (evenp (length options)))
    (error 'type-error :datum options :expected-type 'list))
  (let ((result nil))
    (loop for (key value) on options by #'cddr
          do (unless (member key +icalendar-option-keys+ :test #'eq)
               (error 'org-export-error
                      :message
                      "Unknown iCalendar option"
                      :backend
                      :icalendar
                      :reason
                      :invalid-option)) (let ((entry (assoc key result)))
                                          (if entry
                                              (setf (cdr entry) (%icalendar-parse-option-value
                                                                 key
                                                                 value))
                                              (setf result (append result
                                                                   (list
                                                                    (cons key
                                                                          (%icalendar-parse-option-value
                                                                           key
                                                                           value))))))))
    result))

(defun %icalendar-keyword-options (document)
  (let ((result nil))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :keyword)
         (let* ((raw-key (node-property node :key))
                (value (node-property node :value))
                (key (%icalendar-keyword-option-name raw-key)))
           (when (and (stringp raw-key) (string-equal raw-key "BIND"))
             (multiple-value-setq (key value) (%icalendar-bind-option value)))
           (when key
             (when (assoc key result)
               (export-unsupported-syntax :icalendar
                                          node
                                          (node-begin node)
                                          :duplicate-keyword
                                          "Duplicate iCalendar keyword"))
             (push (cons key (%icalendar-parse-option-value key value)) result)))))
     (document-root document))
    (nreverse result)))

(defun %icalendar-document-keyword-value (document name)
  (let ((value nil)
        (found-p nil))
    (map-nodes
     (lambda (node)
       (when
           (and (not found-p)
                (eq (node-type node) :keyword)
                (stringp (node-property node :key))
                (string-equal (node-property node :key) name))
         (setf value (node-property node :value)
               found-p t)))
     (document-root document))
    (values value found-p)))

(defun %icalendar-source-base-name (document)
  (let ((source (document-source-name document)))
    (when (and (stringp source) (plusp (length source)))
      (let* ((start
              (1+
               (or
                (position-if
                 (lambda (character)
                   (find character "/\\"))
                 source
                 :from-end
                 t)
                -1)))
             (base (subseq source start))
             (dot (position #\. base :from-end t)))
        (if dot
            (subseq base 0 dot)
            base)))))

(defun %icalendar-category-name (document)
  (or (getf (document-settings document) :category)
      (%icalendar-source-base-name document)
      "OrgMode"))

(defun %icalendar-calendar-name (documents options)
  (let* ((document (first documents))
         (keyword-name
          (and document
               (assoc :combined-name (%icalendar-keyword-options document))))
         (explicit-name (assoc :combined-name options)))
    (or (and explicit-name (cdr explicit-name))
        (and keyword-name (cdr keyword-name))
        (and (= (length documents) 1) (%icalendar-source-base-name document))
        (or (%icalendar-option options :combined-name) "OrgMode"))))

(defun %icalendar-options (document explicit-options)
  (let ((result (copy-tree +icalendar-option-defaults+)))
    (dolist (entry (%icalendar-keyword-options document))
      (setf (cdr (assoc (car entry) result)) (cdr entry)))
    (dolist (entry explicit-options)
      (setf (cdr (assoc (car entry) result)) (cdr entry)))
    (%icalendar-validate-store-uid result)))

(defun %icalendar-option (options key)
  (cdr (assoc key options :test #'eq)))

(defun %icalendar-validate-store-uid (options)
  (when (%icalendar-option options :store-uid)
    (error 'org-export-error
           :message
           "Persisting generated iCalendar UIDs is unsupported"
           :backend
           :icalendar
           :reason
           :unsupported-option))
  options)

(defun %icalendar-property-options (document heading options)
  (let ((result (copy-tree options)))
    (dolist (key +icalendar-option-keys+)
      (multiple-value-bind (value found-p)
          (effective-property document
                              heading
                              (format nil
                                      "ICALENDAR_~A"
                                      (substitute #\_ #\- (symbol-name key))))
        (when found-p
          (let ((entry (assoc key result)))
            (when entry
              (setf (cdr entry) (%icalendar-parse-option-value key value)))))))
    (%icalendar-validate-store-uid result)))

(defun %icalendar-date-value (value &optional exclusive-date-p)
  (let* ((year (org-time-value-year value))
         (month (org-time-value-month value))
         (day (org-time-value-day value)))
    (when exclusive-date-p
      (let* ((date (cl-date-kit:make-local-date year month day))
             (next (cl-date-kit:local-date-plus-days date 1)))
        (setf year (cl-date-kit:local-date-year next)
              month (cl-date-kit:local-date-month next)
              day (cl-date-kit:local-date-day next))))
    (format nil "~4,'0D~2,'0D~2,'0D" year month day)))

(defun %icalendar-date-time-value (value)
  (format nil
          "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0D"
          (org-time-value-year value)
          (org-time-value-month value)
          (org-time-value-day value)
          (org-time-value-hour value)
          (org-time-value-minute value)
          (or (org-time-value-second value) 0)))

(defun %icalendar-dtstamp (time-context)
  (unless (org-time-context-p time-context)
    (%org-time-error :invalid-time "iCalendar export requires a time context"))
  (let* ((instant (org-time-context-reference-instant time-context))
         (utc-zone (cl-date-kit:zone-offset-of-total-seconds 0))
         (utc (cl-date-kit:local-date-time-of-instant instant utc-zone)))
    (format nil
            "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0DZ"
            (cl-date-kit:local-date-time-year utc)
            (cl-date-kit:local-date-time-month utc)
            (cl-date-kit:local-date-time-day utc)
            (cl-date-kit:local-date-time-hour utc)
            (cl-date-kit:local-date-time-minute utc)
            (cl-date-kit:local-date-time-second utc))))

(defun %icalendar-random-octet (random-source)
  (let ((octet (funcall random-source)))
    (unless (typep octet '(unsigned-byte 8))
      (error 'org-export-error
             :message
             "Invalid iCalendar random source"
             :backend
             :icalendar
             :reason
             :invalid-random-source))
    octet))

(defun %icalendar-uid (heading random-source)
  (let ((identifier
         (and heading (effective-property (node-document heading) heading "ID"))))
    (if identifier
        identifier
        (progn
          (unless (functionp random-source)
            (error 'org-export-error
                   :message
                   "iCalendar UID generation needs a random source"
                   :backend
                   :icalendar
                   :reason
                   :missing-random-source))
          (let ((hex "0123456789abcdef"))
            (with-output-to-string (output)
              (dotimes (index 16)
                (let* ((source-octet (%icalendar-random-octet random-source))
                       (octet
                        (case index
                          (6 (logior #x40 (logand source-octet #x0f)))
                          (8 (logior #x80 (logand source-octet #x3f)))
                          (otherwise source-octet))))
                  (when (member index '(4 6 8 10))
                    (write-char #\- output))
                  (write-char (char hex (ash octet -4)) output)
                  (write-char (char hex (logand octet #x0f)) output)))))))))

(defun %icalendar-headline-title (heading)
  (let ((objects (node-property heading :title)))
    (if objects
        (with-output-to-string (output)
          (dolist (object objects)
            (write-string
             (if (stringp object)
                 object
                 (%icalendar-node-text object))
             output)))
        (or (node-property heading :raw-value) ""))))

(defun %icalendar-node-text (node &optional options)
  (let ((type (node-type node)))
    (case type
      ((:plain-text :text :code :verbatim :inline-src-block)
       (or (node-text node) (node-property node :value) ""))
      (:entity
       (or (node-property node :utf-8)
           (node-property node :unicode)
           (node-text node)
           ""))
      (:line-break (string #\Newline))
      (:timestamp
       (let* ((mode
               (if options
                   (%icalendar-option options :with-timestamps)
                   :active))
              (timestamp-type (node-property node :type)))
         (if (case mode
               ((:active :active-exclude-diary t)
                (member timestamp-type '(:active :active-range) :test #'eq))
               (:inactive
                (member timestamp-type '(:inactive :inactive-range) :test #'eq))
               (:all
                (member timestamp-type
                        '(:active :active-range :inactive :inactive-range)
                        :test
                        #'eq))
               (otherwise nil))
             (or (node-text node) (node-property node :value) "")
             "")))
      (:link
       (if (node-children node)
           (with-output-to-string (output)
             (dolist (child (node-children node))
               (write-string (%icalendar-node-text child options) output)))
           (or (node-property node :path) "")))
      ((:headline :org-data
                  :section
                  :property-drawer
                  :planning
                  :keyword
                  :node-property
                  :drawer
                  :diary-sexp) "")
      (otherwise
       (if (node-children node)
           (with-output-to-string (output)
             (dolist (child (node-children node))
               (write-string (%icalendar-node-text child options) output)))
           (or (node-text node) (node-property node :value) ""))))))

(defun %icalendar-headline-body (heading options limits)
  (labels ((normalize-linebreaks (text)
             (with-output-to-string (output)
               (loop for index from 0 below (length text)
                     for character = (char text index)
                     do (cond
                          ((char= character #\Return)
                           (when (and (< (1+ index) (length text))
                                      (char= (char text (1+ index)) #\Newline))
                             (incf index))
                           (write-string "  " output))
                          ((char= character #\Newline)
                           (write-string "  " output))
                          (t
                           (write-char character output))))))
           (fill-text (text width)
             (let ((position 0)
                   (line-width 0))
               (with-output-to-string (output)
                 (loop while (< position (length text))
                       for spaces-start = position
                       do (loop while (and (< position (length text))
                                           (or (char= (char text position) #\Space)
                                               (char= (char text position) #\Tab)))
                                do (incf position))
                          (let ((word-start position))
                            (loop while (and (< position (length text))
                                             (not (or (char= (char text position) #\Space)
                                                      (char= (char text position) #\Tab))))
                                  do (incf position))
                            (let* ((word-end position)
                                   (spaces (subseq text spaces-start word-start))
                                   (word (subseq text word-start word-end))
                                   (spaces-width
                                     (export-ascii-output-text-width spaces))
                                   (word-width
                                     (export-ascii-output-text-width word)))
                              (when (plusp (length word))
                                (if (and (plusp line-width)
                                         (> (+ line-width spaces-width word-width)
                                            width))
                                    (progn
                                      (terpri output)
                                      (write-string word output)
                                      (setf line-width word-width))
                                    (progn
                                      (write-string spaces output)
                                      (write-string word output)
                                      (incf line-width (+ spaces-width word-width))))))))))
    ))
    (let ((section (find :section (node-children heading) :key #'node-type)))
      (when (and section (%icalendar-option options :include-body))
        (let ((span
               (and (node-begin section)
                    (node-end section)
                    (- (node-end section) (node-begin section)))))
          (when span
            (check-limit limits :max-output-length span)))
        (let* ((include-body (%icalendar-option options :include-body))
               (paragraphs
                (loop for child in (node-children section)
                      unless (member (node-type child)
                                     '(:property-drawer :planning :keyword)
                                     :test #'eq)
                      collect (%icalendar-node-text child options)))
               (body
                (fill-text
                 (normalize-linebreaks
                  (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (format nil "~{~A~^~%~}" paragraphs)))
                 (export-ascii-current-width section))))
          (if (integerp include-body)
              (subseq body 0 (min (length body) include-body))
            body))))))

(defun %icalendar-heading-tags (heading)
  (or (node-property heading :tags) nil))

(defun %icalendar-excluded-heading-p (heading options)
  (let ((excluded (%icalendar-option options :exclude-tags)))
    (some
     (lambda (tag)
       (some
        (lambda (excluded-tag)
          (string-equal tag
                        (if (stringp excluded-tag)
                            excluded-tag
                            (symbol-name excluded-tag))))
        excluded))
     (%icalendar-heading-tags heading))))

(defun %icalendar-categories (document heading options)
  (let ((values nil))
    (dolist (category (%icalendar-option options :categories))
      (let ((name
             (if (stringp category)
                 (string-downcase category)
                 (string-downcase (symbol-name category)))))
        (cond
          ((string= name "local-tags")
           (setf values (append values (%icalendar-heading-tags heading))))
          ((string= name "category")
           (setf values (append values
                                (list (%icalendar-category-name document)))))
          ((string= name "todo-state")
           (let ((state (node-property heading :todo-keyword)))
             (when state
               (setf values (append values (list state))))))
          ((string= name "all-tags")
           (multiple-value-bind (tags found-p)
               (effective-property document heading "TAGS" :inherit t)
             (when found-p
               (setf values (append values (export-split-words tags)))))))))
    (remove-duplicates values :test (function string-equal) :from-end t)))

(defun %icalendar-timestamp-list (heading options)
  (let ((mode (%icalendar-option options :with-timestamps))
        (result nil))
    (labels ((selected-p (type)
               (case mode
                 ((:active t) (member type '(:active :active-range) :test #'eq))
                 (:active-exclude-diary
                  (member type '(:active :active-range) :test #'eq))
                 (:inactive
                  (member type '(:inactive :inactive-range) :test #'eq))
                 (:all
                  (member type
                          '(:active :active-range :inactive :inactive-range)
                          :test
                          #'eq))
                 (otherwise nil)))
             (visit (node)
               (dolist (child (node-children node))
                 (unless
                     (or
                      (eq (node-type child) :headline)
                      (member (node-type child)
                              '(:planning :property-drawer :node-property)
                              :test
                              #'eq))
                   (if (and (eq (node-type child) :timestamp)
                            (selected-p (node-property child :type)))
                       (push child result)
                       (visit child))))))
      (dolist (object (node-property heading :title))
        (when
            (and (node-p object)
                 (eq (node-type object) :timestamp)
                 (selected-p (node-property object :type)))
          (push object result)))
      (visit heading))
    (nreverse result)))

(defun %icalendar-todo-included-p (heading options)
  (let ((include (%icalendar-option options :include-todo))
        (todo-type (node-property heading :todo-type))
        (todo-keyword (node-property heading :todo-keyword)))
    (and todo-keyword
         (cond
           ((eq include :all) t)
           ((eq include :unblocked)
            (not
             (or (org-todo-unfinished-child-p heading)
                 (org-todo-ordered-sibling-blocked-p heading))))
           ((eq include t) (eq todo-type :todo))
           ((listp include) (member todo-keyword include :test #'string-equal))))))

(defun %icalendar-entry-event-enabled-p (todo-type modes)
  (cond
    ((null todo-type) (member :event-if-not-todo modes))
    ((eq todo-type :todo)
     (or (member :event-if-todo modes) (member :event-if-todo-not-done modes)))
    ((eq todo-type :done) (member :event-if-todo modes))))

(defun %icalendar-planning-events (heading options time-context)
  (labels ((timestamp-for-local (local)
             (let ((value (%time-value-from-local-value local)))
               (make-node :timestamp
                          :properties
                          (append
                           (list :type
                                 :active
                                 :year-start
                                 (org-time-value-year value)
                                 :month-start
                                 (org-time-value-month value)
                                 :day-start
                                 (org-time-value-day value))
                           (when (eq (org-time-value-kind value) :date-time)
                             (list :hour-start
                                   (org-time-value-hour value)
                                   :minute-start
                                   (org-time-value-minute value)))))))
           (warning-days (timestamp)
             (let* ((view (timestamp-view timestamp))
                    (amount (org-timestamp-view-warning-amount view))
                    (unit (org-timestamp-view-warning-unit view)))
               (if amount
                   (floor
                    (* amount
                       (case unit
                         (:hour 0.041667)
                         (:day 1)
                         (:week 7)
                         (:month 30.4)
                         (:year 365.25))))
                   14)))
           (deadline-warning-start (timestamp)
             (let* ((start (timestamp-start timestamp))
                    (days (warning-days timestamp)))
               (timestamp-for-local
                (if (cl-date-kit:local-date-time-p start)
                    (cl-date-kit:local-date-time-plus-days start (- days))
                    (cl-date-kit:local-date-plus-days start (- days))))))
           (unscheduled-start (deadline)
             (case (%icalendar-option options :todo-unscheduled-start)
               (:current-datetime
                 (unless (org-time-context-p time-context)
                   (%org-time-error :invalid-time
                                    "TODO start requires a time context"))
                 (timestamp-for-local
                  (cl-date-kit:local-date-time-of-instant
                   (org-time-context-reference-instant time-context)
                   (org-time-zone-backend (org-time-context-zone time-context)))))
               (:deadline-warning
                (and deadline (deadline-warning-start deadline)))
               (:recurring-deadline-warning
                (and deadline
                     (%icalendar-repeater-rule deadline t)
                     (deadline-warning-start deadline)))
               (otherwise nil))))
    (let* ((todo-keyword (node-property heading :todo-keyword))
           (todo-type (node-property heading :todo-type))
           (todo (not (null todo-keyword)))
           (scheduled (node-property heading :scheduled))
           (deadline (node-property heading :deadline))
           (use-scheduled (%icalendar-option options :use-scheduled))
           (use-deadline (%icalendar-option options :use-deadline))
           (include-todo (%icalendar-todo-included-p heading options))
           (scheduled-event
            (and (node-p scheduled)
                 (eq (node-type scheduled) :timestamp)
                 (%icalendar-entry-event-enabled-p todo-type use-scheduled)))
           (deadline-event
            (and (node-p deadline)
                 (eq (node-type deadline) :timestamp)
                 (%icalendar-entry-event-enabled-p todo-type use-deadline)))
           (todo-scheduled
            (when (and todo include-todo)
              (or (and (member :todo-start use-scheduled) scheduled)
                  (unscheduled-start
                   (and (member :todo-due use-deadline) deadline)))))
           (todo-deadline (and (member :todo-due use-deadline) deadline))
           (active-timestamps (%icalendar-timestamp-list heading options)))
      (append
       (when deadline-event
         (list
          (list :kind
                :event
                :uid-prefix
                "DL-"
                :deadline
                deadline
                :timestamp
                deadline)))
       (when scheduled-event
         (list
          (list :kind
                :event
                :uid-prefix
                "SC-"
                :scheduled
                scheduled
                :timestamp
                scheduled)))
       (loop for timestamp in active-timestamps
             for index from 1
             collect (list :kind
                           :event
                           :uid-prefix
                           (format nil "TS~D-" index)
                           :timestamp
                           timestamp))
       (when (and todo include-todo)
         (list
          (list :kind
                :todo
                :uid-prefix
                "TODO-"
                :scheduled
                todo-scheduled
                :deadline
                todo-deadline
                :timestamp
                (or (and (member :todo-start use-scheduled) scheduled)
                    todo-deadline))))))))

(defun %icalendar-repeater-rule (timestamp &optional todo-p)
  (when
      (and timestamp
           (or (not todo-p)
               (eq (node-property timestamp :repeater-type) :cumulate)))
    (let ((amount (node-property timestamp :repeater-value))
          (unit (node-property timestamp :repeater-unit))
          (mode (node-property timestamp :repeater-type)))
      (when (and mode (integerp amount) (plusp amount))
        (let ((frequency
               (case unit
                 (:hour "HOURLY")
                 (:day "DAILY")
                 (:week "WEEKLY")
                 (:month "MONTHLY")
                 (:year "YEARLY"))))
          (when frequency
            (format nil "FREQ=~A;INTERVAL=~D" frequency amount)))))))

(defun %icalendar-zone-name (time-context options)
  (let ((zone
         (let ((override (%icalendar-option options :timezone)))
           (or (and override (plusp (length override)) override)
               (and (org-time-context-p time-context)
                    (org-time-zone-name (org-time-context-zone time-context)))
               "UTC"))))
    (unless (%icalendar-safe-zone-name-p zone)
      (error 'org-export-error
             :message
             "Invalid iCalendar time zone"
             :backend
             :icalendar
             :reason
             :invalid-timezone))
    zone))

(defun %icalendar-safe-zone-name-p (name)
  (or (null name)
      (and (stringp name)
           (plusp (length name))
           (every
            (lambda (character)
              (or
               (and (<= (char-code character) #x7f)
                    (or (char<= #\A character #\Z)
                        (char<= #\a character #\z)
                        (char<= #\0 character #\9)))
               (find character "/_+-")))
            name))))

(defun %icalendar-event-end (timestamp start)
  (let ((end (timestamp-end timestamp)))
    (cond
      ((and end (not (equalp end start))) end)
      ((cl-date-kit:local-date-time-p start)
       (cl-date-kit:local-date-time-plus-hours start 2))
      (t (or end start)))))

(defun %icalendar-timestamp-property (builder name
                                              timestamp
                                              zone
                                              options
                                              time-context
                                              &key
                                              exclusive-end-p)
  (labels ((format-part (pattern value format-zone date-time-p)
             (with-output-to-string (output)
               (loop for index from 0 below (length pattern)
                     for character = (char pattern index)
                     do (if (char= character #\%)
                            (progn
                              (incf index)
                              (when (>= index (length pattern))
                                (error 'org-export-error
                                       :message
                                       "Invalid iCalendar date-time format"
                                       :backend
                                       :icalendar
                                       :reason
                                       :invalid-option))
                              (case (char pattern index)
                                (#\% (write-char #\% output))
                                (#\Z (write-string format-zone output))
                                ((#\Y #\y #\m #\d #\H #\M #\S)
                                  (unless (and date-time-p value)
                                    (error 'org-export-error
                                           :message
                                           "Invalid iCalendar date-time format"
                                           :backend
                                           :icalendar
                                           :reason
                                           :invalid-option))
                                  (write-string
                                   (case (char pattern index)
                                     (#\Y
                                      (format nil
                                              "~4,'0D"
                                              (org-time-value-year value)))
                                     (#\y
                                      (format nil
                                              "~2,'0D"
                                              (mod (org-time-value-year value)
                                                   100)))
                                     (#\m
                                      (format nil
                                              "~2,'0D"
                                              (org-time-value-month value)))
                                     (#\d
                                      (format nil
                                              "~2,'0D"
                                              (org-time-value-day value)))
                                     (#\H
                                      (format nil
                                              "~2,'0D"
                                              (org-time-value-hour value)))
                                     (#\M
                                      (format nil
                                              "~2,'0D"
                                              (org-time-value-minute value)))
                                     (#\S
                                      (format nil
                                              "~2,'0D"
                                              (or (org-time-value-second value)
                                                  0))))
                                   output))
                                (otherwise
                                 (error 'org-export-error
                                        :message
                                        "Unsupported iCalendar date-time directive"
                                        :backend
                                        :icalendar
                                        :reason
                                        :invalid-option))))
                            (write-char character output)))))
           (utc-value (value)
             (unless (org-time-context-p time-context)
               (%org-time-error :invalid-time
                                "UTC iCalendar timestamps require a time context"))
             (let* ((instant
                     (%time-value-to-instant value
                                             (org-time-context-zone
                                              time-context)
                                             (org-time-context-dst-policy
                                              time-context)))
                    (local
                     (cl-date-kit:local-date-time-of-instant instant
                                                             (cl-date-kit:zone-offset-of-total-seconds
                                                              0))))
               (%time-value-from-local-value local))))
    (let* ((start (timestamp-start timestamp))
           (end (and start (%icalendar-event-end timestamp start)))
           (local
            (if exclusive-end-p
                (or end start)
                start)))
      (unless local
        (%org-time-error :invalid-time
                         "iCalendar event has an invalid timestamp"))
      (let* ((value (%time-value-from-local-value local))
             (date-p (eq (org-time-value-kind value) :date))
             (format-zone (%icalendar-zone-name time-context options))
             (format-string (%icalendar-option options :date-time-format)))
        (cond
          ((and zone (string-equal zone "UTC"))
            (progn
              (when (and exclusive-end-p date-p)
                (setf value (%time-value-from-local-value
                             (cl-date-kit:local-date-plus-days local 1))))
              (setf value (utc-value value)))
            (%icalendar-append-line builder
                                    (list (cons name nil)
                                          (cons ":" nil)
                                          (cons
                                           (concatenate 'string
                                                        (%icalendar-date-time-value
                                                         value)
                                                        "Z")
                                           nil))))
          ((and zone date-p)
           (%icalendar-append-line builder
                                   (list (cons name nil)
                                         (cons ";VALUE=DATE" nil)
                                         (cons ":" nil)
                                         (cons
                                          (%icalendar-date-value value
                                                                 exclusive-end-p)
                                          nil))))
          (zone
           (%icalendar-append-line builder
                                   (list (cons name nil)
                                         (cons (format nil ";TZID=~A" zone) nil)
                                         (cons ":" nil)
                                         (cons
                                          (%icalendar-date-time-value value)
                                          nil))))
          (date-p
           (%icalendar-append-line builder
                                   (list (cons name nil)
                                         (cons ";VALUE=DATE" nil)
                                         (cons ":" nil)
                                         (cons
                                          (%icalendar-date-value value
                                                                 exclusive-end-p)
                                          nil))))
          (t
           (let* ((separator
                   (and (stringp format-string) (position #\: format-string)))
                  (prefix (and separator (subseq format-string 0 separator)))
                  (pattern
                   (and separator (subseq format-string (1+ separator))))
                  (utc-p
                   (and pattern
                        (plusp (length pattern))
                        (char= (char pattern (1- (length pattern))) #\Z))))
             (unless separator
               (error 'org-export-error
                      :message
                      "Invalid iCalendar date-time format"
                      :backend
                      :icalendar
                      :reason
                      :invalid-option))
             (when (some #'export-control-character-p format-string)
               (error 'org-export-error
                      :message
                      "Invalid iCalendar date-time format"
                      :backend
                      :icalendar
                      :reason
                      :invalid-option))
             (when utc-p
               (setf pattern (subseq pattern 0 (1- (length pattern)))
                     value (utc-value value)))
             (%icalendar-append-line builder
                                     (list (cons name nil)
                                           (cons
                                            (format-part prefix
                                                         nil
                                                         format-zone
                                                         nil)
                                            nil)
                                           (cons ":" nil)
                                           (cons
                                            (concatenate 'string
                                                         (format-part pattern
                                                                      value
                                                                      format-zone
                                                                      t)
                                                         (if utc-p
                                                             "Z"
                                                             ""))
                                            nil))))))))))

(defun %icalendar-event-time-properties (builder event
                                                 zone
                                                 options
                                                 time-context)
  (let* ((timestamp (getf event :timestamp))
         (scheduled (getf event :scheduled))
         (deadline (getf event :deadline)))
    (cond
      ((eq (getf event :kind) :todo)
        (when scheduled
          (%icalendar-timestamp-property builder
                                         "DTSTART"
                                         scheduled
                                         zone
                                         options
                                         time-context))
        (when deadline
          (%icalendar-timestamp-property builder
                                         "DUE"
                                         deadline
                                         zone
                                         options
                                         time-context)))
      (timestamp
        (%icalendar-timestamp-property builder
                                       "DTSTART"
                                       timestamp
                                       zone
                                       options
                                       time-context)
        (%icalendar-timestamp-property builder
                                       "DTEND"
                                       timestamp
                                       zone
                                       options
                                       time-context
                                       :exclusive-end-p
                                       t)))))

(defun %icalendar-summary-prefix (event options)
  (if (eq (getf event :kind) :todo)
      ""
      (cond
        ((getf event :scheduled)
         (%icalendar-option options :summary-prefix-scheduled))
        ((getf event :deadline)
         (%icalendar-option options :summary-prefix-deadline))
        (t ""))))

(defun %icalendar-event-alarms (builder heading event summary options)
  (let* ((configured (%icalendar-option options :alarm-time))
         (force-p (%icalendar-option options :force-alarm))
         (timestamp
          (or (getf event :deadline)
              (getf event :scheduled)
              (getf event :timestamp)))
         (property
          (effective-property (node-document heading) heading "APPT_WARNTIME"))
         (minutes
          (and property
               (ignore-errors (parse-integer property :junk-allowed nil))))
         (alarm-minutes
          (cond
            ((and minutes (plusp minutes)) minutes)
            ((plusp configured) configured)
            (force-p (or minutes configured))
            (t nil))))
    (when (and timestamp (node-property timestamp :hour-start) alarm-minutes)
      (%icalendar-append-line builder (list (cons "BEGIN:VALARM" nil)))
      (%icalendar-line builder "ACTION" "DISPLAY")
      (%icalendar-line builder "DESCRIPTION" summary :text-value-p t)
      (%icalendar-line builder
                       "TRIGGER"
                       (format nil "-P0DT0H~DM0S" alarm-minutes))
      (%icalendar-append-line builder (list (cons "END:VALARM" nil)))))
  builder)

(defun %icalendar-render-event (builder document
                                        heading
                                        event
                                        options
                                        time-context
                                        uid-base
                                        zone)
  (declare (ignore zone))
  (let* ((event-options (%icalendar-property-options document heading options))
         (uid (concatenate 'string (getf event :uid-prefix) uid-base))
         (event-zone (effective-property document heading "TIMEZONE"))
         (timestamp-text (%icalendar-dtstamp time-context))
         (title
          (or (effective-property document heading "SUMMARY")
              (%icalendar-headline-title heading)))
         (prefix (%icalendar-summary-prefix event event-options))
         (body
          (or (effective-property document heading "DESCRIPTION")
              (%icalendar-headline-body heading
                                        event-options
                                        (%icalendar-builder-limits builder))))
         (location (effective-property document heading "LOCATION"))
         (class (effective-property document heading "CLASS"))
         (summary (concatenate 'string prefix title)))
    (unless (%icalendar-safe-zone-name-p event-zone)
      (error 'org-export-error
             :message
             "Invalid iCalendar time zone"
             :backend
             :icalendar
             :reason
             :invalid-timezone))
    (%icalendar-append-line builder
                            (list
                             (cons
                              (if (eq (getf event :kind) :todo)
                                  "BEGIN:VTODO"
                                  "BEGIN:VEVENT")
                              nil)))
    (when (eq (getf event :kind) :todo)
      (%icalendar-line builder "UID" uid :text-value-p t))
    (%icalendar-line builder "DTSTAMP" timestamp-text)
    (unless (eq (getf event :kind) :todo)
      (%icalendar-line builder "UID" uid :text-value-p t))
    (%icalendar-event-time-properties builder
                                      event
                                      event-zone
                                      event-options
                                      time-context)
    (let ((rule
           (%icalendar-repeater-rule (getf event :timestamp)
                                     (eq (getf event :kind) :todo))))
      (when rule
        (%icalendar-line builder "RRULE" rule)))
    (%icalendar-append-line builder
                            (list (cons "SUMMARY:" nil) (cons summary t)))
    (when (and location (plusp (length location)))
      (%icalendar-line builder "LOCATION" location :text-value-p t))
    (when (and class (plusp (length class)))
      (%icalendar-line builder "CLASS" class :text-value-p t))
    (when (and body (plusp (length body)))
      (%icalendar-line builder "DESCRIPTION" body :text-value-p t))
    (let ((categories (%icalendar-categories document heading event-options)))
      (%icalendar-append-line builder
                              (append (list (cons "CATEGORIES:" nil))
                                      (loop for category in categories
                                            for first-p = t then nil
                                            append (append
                                                    (unless first-p
                                                      (list (cons "," nil)))
                                                    (list (cons category t)))))))
    (when (eq (getf event :kind) :todo)
      (%icalendar-line builder "SEQUENCE" "1")
      (%icalendar-line builder "PRIORITY" (%icalendar-org-priority heading))
      (%icalendar-line builder
                       "STATUS"
                       (if (eq (node-property heading :todo-type) :todo)
                           "NEEDS-ACTION"
                           "COMPLETED")))
    (unless (eq (getf event :kind) :todo)
      (%icalendar-event-alarms builder heading event summary event-options))
    (%icalendar-append-line builder
                            (list
                             (cons
                              (if (eq (getf event :kind) :todo)
                                  "END:VTODO"
                                  "END:VEVENT")
                              nil))))
  builder)

(defun %icalendar-org-priority (heading)
  (let* ((raw-priority (or (node-property heading :priority) #\B))
         (priority
          (if (characterp raw-priority)
              (char-code raw-priority)
              raw-priority))
         (high (char-code #\A))
         (low (char-code #\C)))
    (format nil
            "~D"
            (max 0
                 (min 9
                      (floor
                       (- 9 (* 8 (/ (float (- low priority)) (- low high))))))))))

(defun %icalendar-render-document-events (builder document
                                                  explicit-options
                                                  time-context
                                                  random-source
                                                  zone)
  (let ((options (%icalendar-options document explicit-options))
        (root-diary-index 0))
    (labels ((diary-node-p (node)
               (or (eq (node-type node) :diary-sexp)
                   (and (eq (node-type node) :timestamp)
                        (eq (node-property node :type) :diary))))
             (diary-block-data (node)
               (handler-case (let* ((source
                                     (diary-sexp-source-for-node node
                                                                 (document-limits
                                                                  document)))
                                    (form
                                     (and source
                                          (%diary-sexp-source-form source)))
                                    (arguments (and form (cdr form))))
                               (when
                                   (and source
                                        (string=
                                         (or
                                          (%diary-sexp-source-function-name
                                           source)
                                          "")
                                         "diary-block")
                                        (= (length arguments) 6)
                                        (every #'integerp arguments))
                                 (let* ((start
                                         (diary-sexp-exact-date-ordinal
                                          (first arguments)
                                          (second arguments)
                                          (third arguments)))
                                        (end
                                         (diary-sexp-exact-date-ordinal
                                          (fourth arguments)
                                          (fifth arguments)
                                          (sixth arguments))))
                                   (when (and start end (<= start end))
                                     (list source arguments)))))
                 (org-agenda-error ()
                   nil)))
             (unsupported-diary-sexp (node)
               (export-unsupported-syntax :icalendar
                                          node
                                          (export-source-position node)
                                          :diary-sexp
                                          "Only literal diary-block date ranges are supported"))
             (collect-diary-nodes (node heading result)
               (let ((result
                      (if (diary-node-p node)
                          (cons node result)
                          result)))
                 (dolist (child (node-children node) result)
                   (unless
                       (eq (node-type child) :headline)
                     (setf result (collect-diary-nodes child heading result))))))
             (heading-diary-nodes (heading)
               (let ((result nil))
                 (dolist (object (node-property heading :title))
                   (when (node-p object)
                     (setf result (collect-diary-nodes object heading result))))
                 (setf result (collect-diary-nodes heading heading result))
                 (nreverse (remove-duplicates result :test #'eq :from-end t))))
             (render-diary-block (node heading index)
               (let ((data (diary-block-data node)))
                 (unless data
                   (unsupported-diary-sexp node))
                 (let* ((source (first data))
                        (arguments (second data))
                        (timestamp-p (eq (node-type node) :timestamp))
                        (entry
                         (string-left-trim '(#\Space #\Tab)
                                           (%diary-sexp-source-entry source)))
                        (uid
                         (let ((identifier
                                (and heading
                                     (effective-property document heading "ID"))))
                           (if identifier
                               (format nil "DS~D-~A" index identifier)
                               (%icalendar-uid nil random-source))))
                        (summary
                         (if (and timestamp-p (zerop (length entry)))
                             (or
                              (and heading
                                   (effective-property document
                                                       heading
                                                       "SUMMARY"))
                              (and heading (%icalendar-headline-title heading))
                              "")
                             entry))
                        (description
                         (or
                          (and heading
                               (effective-property document
                                                   heading
                                                   "DESCRIPTION"))
                          summary))
                        (start-date
                         (format nil
                                 "~4,'0D~2,'0D~2,'0D"
                                 (third arguments)
                                 (first arguments)
                                 (second arguments)))
                        (end-date
                         (format nil
                                 "~4,'0D~2,'0D~2,'0D"
                                 (sixth arguments)
                                 (fourth arguments)
                                 (fifth arguments))))
                   (%icalendar-append-line builder
                                           (list (cons "BEGIN:VEVENT" nil)))
                   (%icalendar-line builder "UID" uid :text-value-p t)
                   (%icalendar-line builder
                                    "DTSTAMP"
                                    (%icalendar-dtstamp time-context))
                   (%icalendar-line builder "SUMMARY" summary :text-value-p t)
                   (%icalendar-line builder
                                    "DESCRIPTION"
                                    description
                                    :text-value-p
                                    t)
                   (%icalendar-line builder
                                    "RRULE"
                                    (format nil "FREQ=DAILY;UNTIL=~A" end-date))
                   (%icalendar-line builder "DTSTART;VALUE=DATE" start-date)
                   (%icalendar-append-line builder
                                           (list (cons "END:VEVENT" nil)))))))
      (map-nodes
       (lambda (node)
         (when
             (and (diary-node-p node)
                  (let ((heading
                         (loop for current = node then (node-parent current)
                               while current
                               when (eq (node-type current) :headline)
                                 return current)))
                    (let ((node-options
                           (if heading
                               (%icalendar-property-options document
                                                            heading
                                                            options)
                               options)))
                      (when (%icalendar-option node-options :include-sexps)
                        (unless (diary-block-data node)
                          (unsupported-diary-sexp node))
                        (unless heading
                          (incf root-diary-index)
                          (render-diary-block node nil root-diary-index))))))))
       (document-root document))
      (map-nodes
       (lambda (heading)
         (when (eq (node-type heading) :headline)
           (let ((heading-options
                  (%icalendar-property-options document heading options)))
             (when
                 (and (not (node-property heading :commentedp))
                      (not (node-property heading :archivedp))
                      (not
                       (%icalendar-excluded-heading-p heading heading-options)))
               (let* ((events
                       (%icalendar-planning-events heading
                                                   heading-options
                                                   time-context))
                      (diary-nodes
                       (when (%icalendar-option heading-options :include-sexps)
                         (heading-diary-nodes heading))))
                 (progn
                   (when events
                     (let ((uid-base (%icalendar-uid heading random-source)))
                       (dolist (event events)
                         (%icalendar-render-event builder
                                                  document
                                                  heading
                                                  event
                                                  heading-options
                                                  time-context
                                                  uid-base
                                                  zone))))
                   (loop for node in diary-nodes
                         for index from 1
                         do (render-diary-block node heading index))))))))
       (document-root document)))))

(defun %icalendar-context (context random-source)
  (unless (or (null context) (export-context-p context))
    (error 'type-error :datum context :expected-type '(or null export-context)))
  (let* ((extensions (and context (export-context-extensions context)))
         (extension-source (cdr (assoc :icalendar-random-source extensions)))
         (options (cdr (assoc :icalendar-options extensions))))
    (values (and context (export-context-time-context context))
            (or random-source extension-source)
            options
            (and context (export-context-title context))
            (and context (export-context-author context)))))

(defun %icalendar-output-stream-kind (output-type stream)
  (if (eq output-type :stream)
      (progn
        (unless (and (streamp stream) (output-stream-p stream))
          (error 'org-export-error
                 :message
                 "Stream output requires an output stream"
                 :backend
                 :icalendar
                 :reason
                 :invalid-output))
        (multiple-value-bind (character-p character-known-p)
            (subtypep (stream-element-type stream) 'character)
          (cond
            ((and character-known-p character-p) :character)
            ((equal (stream-element-type stream) '(unsigned-byte 8)) :octets)
            (t
             (error 'org-export-error
                    :message
                    "Unsupported stream element type"
                    :backend
                    :icalendar
                    :reason
                    :invalid-output)))))
      (progn
        (when stream
          (error 'org-export-error
                 :message
                 "A stream is only valid with :STREAM output"
                 :backend
                 :icalendar
                 :reason
                 :invalid-output))
        nil)))

(defun export-icalendar-documents (documents &key
                                             context
                                             random-source
                                             options
                                             (output-type :string)
                                             stream)
  "Combine parsed DOCUMENTS into one RFC 5545 calendar.
CONTEXT supplies the explicit time context. RANDOM-SOURCE is a zero-argument
function returning octets for UIDs that have no ID property. OPTIONS is a plist
of iCalendar option overrides; it takes precedence over in-document keywords."
  (unless (member output-type '(:string :octets :stream) :test #'eq)
    (error 'org-export-error
           :message
           "Unsupported iCalendar output type"
           :backend
           :icalendar
           :reason
           :invalid-output))
  (let ((stream-kind (%icalendar-output-stream-kind output-type stream)))
    (let* ((limits (%icalendar-check-documents documents))
           (explicit-options (%icalendar-explicit-options options)))
      (multiple-value-bind (time-context source
                                         context-options
                                         caller-title
                                         caller-author)
          (%icalendar-context context random-source)
        (let* ((options
                (append (%icalendar-explicit-options context-options)
                        explicit-options))
               (first-options
                (if documents
                    (%icalendar-options (first documents) options)
                    (append (copy-tree +icalendar-option-defaults+) options)))
               (zone (%icalendar-zone-name time-context first-options))
               (title
                (or caller-title
                    (and documents
                         (%icalendar-document-keyword-value (first documents)
                                                            "TITLE"))))
               (author
                (or caller-author
                    (and documents
                         (%icalendar-document-keyword-value (first documents)
                                                            "AUTHOR"))))
               (calendar-name (%icalendar-calendar-name documents options))
               (description
                (or (%icalendar-option first-options :description) title ""))
               (builder (%make-icalendar-builder :limits limits)))
          (%icalendar-append-line builder (list (cons "BEGIN:VCALENDAR" nil)))
          (%icalendar-line builder "VERSION" "2.0")
          (%icalendar-line builder "X-WR-CALNAME" calendar-name :text-value-p t)
          (%icalendar-line builder
                           "PRODID"
                           (format nil
                                   "-//~A//Emacs with Org mode//EN"
                                   (or author ""))
                           :text-value-p
                           t)
          (%icalendar-line builder "X-WR-TIMEZONE" zone)
          (%icalendar-line builder "X-WR-CALDESC" description :text-value-p t)
          (when (%icalendar-option first-options :ttl)
            (%icalendar-line builder
                             "X-PUBLISHED-TTL"
                             (%icalendar-option first-options :ttl)))
          (%icalendar-line builder "CALSCALE" "GREGORIAN")
          (dolist (document documents)
            (%icalendar-render-document-events builder
                                               document
                                               options
                                               time-context
                                               source
                                               zone))
          (%icalendar-append-line builder (list (cons "END:VCALENDAR" nil)))
          (let ((text (coerce (%icalendar-builder-buffer builder) 'string)))
            (case output-type
              (:string text)
              (:octets (cl-codec-kit:string-to-octets text :encoding :utf-8))
              (:stream
                (ecase stream-kind
                  (:character (write-string text stream))
                  (:octets
                   (write-sequence
                    (cl-codec-kit:string-to-octets text :encoding :utf-8)
                    stream)))
                stream))))))))

(defun export-icalendar-render (render-context)
  (let* ((metadata (export-render-context-metadata render-context))
         (document (export-render-context-document render-context))
         (time-context (getf metadata :time-context))
         (random-source
          (export-render-context-extension render-context
                                           :icalendar-random-source))
         (explicit-options
          (export-render-context-extension render-context :icalendar-options)))
    (export-icalendar-documents (list document)
                                :context
                                (make-export-context :title
                                                     (getf metadata :title)
                                                     :author
                                                     (getf metadata :author)
                                                     :time-context
                                                     time-context
                                                     :extensions
                                                     (list
                                                      (cons
                                                       :icalendar-random-source
                                                       random-source)
                                                      (cons :icalendar-options
                                                            explicit-options))))))

(eval-when (:load-toplevel :execute)
  (%register-builtin-export-backend :icalendar
                                    :renderer
                                    #'export-icalendar-render
                                    :accepted-keywords
                                    +icalendar-keyword-names+))

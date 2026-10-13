;;;; src/edit-clock.lisp
;;;; Format-preserving clock-in and clock-out edits.
(in-package #:org-kit)

(defun edit-clock-error (reason &optional context)
  (%org-clock-error reason context))

(defun edit-clock-context-value (at time-context)
  (unless (org-time-value-p at)
    (error 'type-error :datum at :expected-type 'org-time-value))
  (unless (org-time-context-p time-context)
    (error 'type-error :datum time-context :expected-type 'org-time-context))
  at)

(defun edit-clock-timestamp (at time-context)
  (let* ((zone (org-time-context-zone time-context))
         (instant (%time-value-to-instant at zone
                                          (org-time-context-dst-policy time-context)))
         (local (cl-date-kit:local-date-time-of-instant
                 instant (org-time-zone-backend zone)))
         (date (cl-date-kit:local-date-time-date local))
         (weekday
           (cdr (assoc (cl-date-kit:day-of-week date)
                       '((:monday . "Mon") (:tuesday . "Tue")
                         (:wednesday . "Wed") (:thursday . "Thu")
                         (:friday . "Fri") (:saturday . "Sat")
                         (:sunday . "Sun"))))))
    (format nil "[~4,'0D-~2,'0D-~2,'0D ~A ~2,'0D:~2,'0D]"
            (cl-date-kit:local-date-time-year local)
            (cl-date-kit:local-date-time-month local)
            (cl-date-kit:local-date-time-day local)
            weekday
            (cl-date-kit:local-date-time-hour local)
            (cl-date-kit:local-date-time-minute local))))

(defun edit-clock-section (heading)
  (find :section (node-children heading) :key #'node-type))

(defun edit-clock-entry-clock-nodes (heading)
  (let ((clocks '())
        (section (edit-clock-section heading)))
    (when section
      (map-nodes (lambda (node)
                   (when (eq (node-type node) :clock)
                     (push node clocks)))
                 section))
    (nreverse clocks)))

(defun edit-clock-running-nodes (heading)
  (remove-if-not (lambda (clock)
                   (eq (nth-value 0 (clock-row-values clock)) :running))
                 (edit-clock-entry-clock-nodes heading)))

(defun edit-clock-validated-drawer-name (name)
  (when (or (zerop (length name))
            (find #\: name)
            (find #\Newline name)
            (find #\Return name)
            (member (string-upcase name) '("END" "PROPERTIES")
                    :test #'string=))
    (edit-clock-error :unsupported-option (format nil "drawer ~S" name)))
  (string-upcase name))

(defun edit-clock-drawer-name (document heading drawer drawer-supplied-p)
  (let ((setting
          (if drawer-supplied-p
              drawer
              (multiple-value-bind (property present-p)
                  (effective-property document heading "CLOCK_INTO_DRAWER"
                                      :inherit t)
                (cond
                  ((not (and present-p property)) "LOGBOOK")
                  ((and (stringp property)
                        (plusp (length property))
                        (every (lambda (character)
                                 (char<= #\0 character #\9))
                               property))
                   (parse-integer property))
                  (t property))))))
    (cond
      ((typep setting '(integer 0))
       (when (>= (1+ (length (edit-clock-entry-clock-nodes heading))) setting)
         "LOGBOOK"))
      ((string-equal setting "t")
       (multiple-value-bind (log-drawer log-present-p)
           (effective-property document heading "LOG_INTO_DRAWER" :inherit t)
         (if (and log-present-p log-drawer)
             (edit-clock-validated-drawer-name log-drawer)
             "LOGBOOK")))
      (t (edit-clock-validated-drawer-name setting)))))

(defun edit-clock-drawer (heading name)
  (let ((section (and name (edit-clock-section heading))))
    (and section
         (find-if (lambda (node)
                    (and (eq (node-type node) :drawer)
                         (string-equal (node-property node :drawer-name) name)))
                  (node-children section)))))

(defun edit-clock-metadata-p (node)
  (member (node-type node) '(:planning :property-drawer) :test #'eq))

(defun edit-clock-heading-line-end (document heading)
  (let* ((source (document-text document))
         (start (node-begin heading))
         (newline (position #\Newline source :start start)))
    (if newline (1+ newline) (length source))))

(defun edit-clock-drawer-insertion-offset (document heading)
  (let* ((section (edit-clock-section heading))
         (children (and section (node-children section))))
    (cond
      ((null section) (edit-clock-heading-line-end document heading))
      ((find-if-not #'edit-clock-metadata-p children)
       (node-begin (find-if-not #'edit-clock-metadata-p children)))
      (t (node-end (car (last children)))))))

(defun edit-clock-newline (document)
  (let* ((source (document-text document))
         (position (position #\Newline source)))
    (if (and position
             (plusp position)
             (char= (char source (1- position)) #\Return))
        (format nil "~C~C" #\Return #\Newline)
        (string #\Newline))))

(defun edit-clock-insertion-text (document offset drawer-name clock-line)
  (let* ((source (document-text document))
         (newline (edit-clock-newline document))
         (eof-p (= offset (length source)))
         (needs-leading-newline-p
           (and eof-p
                (not (char= (char source (1- offset)) #\Newline))))
         (preserve-final-newline-p
           (and eof-p (document-final-newline-p document))))
    (concatenate 'string
                 (if needs-leading-newline-p newline "")
                 (if drawer-name
                     (concatenate 'string
                                  ":" drawer-name ":" newline
                                  clock-line newline
                                  ":END:")
                     clock-line)
                 (if (or (not eof-p) preserve-final-newline-p) newline ""))))

(defun edit-clock-clock-line-bounds (clock)
  (let* ((document (node-document clock))
         (source (document-text document))
         (start (node-begin clock))
         (newline (position #\Newline source :start start))
         (end (or newline (length source))))
    (when (and newline (> end start)
               (char= (char source (1- end)) #\Return))
      (decf end))
    (values start end)))

(defun edit-clock-drawer-close-offset (document drawer)
  (let* ((source (document-text document))
         (start (node-begin drawer))
         (opening-end (position #\Newline source :start start)))
    (search ":END:" source :start2 (1+ opening-end)
             :end2 (node-end drawer)
             :test #'char-equal)))

(defun edit-clock-drawer-clock-offset (document drawer)
  (or (node-contents-end drawer)
      (edit-clock-drawer-close-offset document drawer)))

(defun edit-clock-canonical-fragment-node (source type &optional drawer-name)
  (let ((document (parse-org source))
        (found nil))
    (map-nodes (lambda (node)
                 (when (and (eq (node-type node) type)
                            (or (null drawer-name)
                                (string-equal (node-property node :drawer-name)
                                              drawer-name)))
                   (setf found node)))
               (document-root document))
    (edit-canonical-node found)))

(defun edit-clock-in-expected (document heading drawer row offset)
  (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
         (clock-canonical
           (edit-clock-canonical-fragment-node (format nil "~A~%" row) :clock))
         (drawer-node (edit-clock-drawer heading drawer)))
    (cond
      (drawer-node
       (let* ((path (edit-node-path document drawer-node))
              (canonical (edit-canonical-node-at-path expected path)))
         (edit-canonical-set-children
          expected path (append (getf canonical :children)
                                (list clock-canonical)))))
      (t
       (let* ((inserted-node
                (if drawer
                    (edit-clock-canonical-fragment-node
                     (format nil ":~A:~%~A~%:END:~%" drawer row)
                     :drawer drawer)
                    clock-canonical))
              (section (edit-clock-section heading)))
         (if section
             (let* ((path (edit-node-path document section))
                    (canonical (edit-canonical-node-at-path expected path))
                    (siblings (node-children section))
                    (index (or (position-if (lambda (node)
                                              (>= (node-begin node) offset))
                                            siblings)
                               (length siblings)))
                    (children (getf canonical :children)))
               (edit-canonical-set-children
                expected path
                (append (subseq children 0 index)
                        (list inserted-node)
                        (subseq children index))))
             (let* ((section-fragment
                      (if drawer
                          (edit-clock-canonical-fragment-node
                           (format nil ":~A:~%~A~%:END:~%" drawer row)
                           :section)
                          (edit-clock-canonical-fragment-node
                           (format nil "~A~%" row) :section)))
                    (path (edit-node-path document heading))
                    (canonical (edit-canonical-node-at-path expected path))
                    (siblings (node-children heading))
                    (index (or (position-if (lambda (node)
                                              (>= (node-begin node) offset))
                                            siblings)
                               (length siblings)))
                    (children (getf canonical :children)))
               (edit-canonical-set-children
                expected path
                (append (subseq children 0 index)
                        (list section-fragment)
                        (subseq children index))))))))
    expected))

(defun clock-in (document heading &key at time-context (drawer nil drawer-supplied-p))
  "Start a CLOCK at AT and return values edit-result and its timestamp.
TIME-CONTEXT is required and supplies the explicit zone and DST policy.
DRAWER overrides CLOCK_INTO_DRAWER; an integer DRAWER is a clock-count threshold."
  (when drawer-supplied-p
    (check-type drawer (or string (integer 0))))
  (clock-check-heading document heading)
  (edit-clock-context-value at time-context)
  (clock-check-document-node-count document)
  (let ((running (edit-clock-running-nodes heading)))
    (when (> (length running) 1)
      (edit-clock-error :multiple-running-clocks))
    (when running
      (edit-clock-error :clock-already-running
                        (clock-row-source-line (first running)))))
  (let* ((drawer-name
           (edit-clock-drawer-name document heading drawer drawer-supplied-p))
         (timestamp (edit-clock-timestamp at time-context))
         (clock-line (format nil "CLOCK: ~A" timestamp))
         (drawer-node (edit-clock-drawer heading drawer-name))
         (patch
           (if drawer-node
               (let* ((offset (edit-clock-drawer-clock-offset document drawer-node))
                      (newline (edit-clock-newline document)))
                 (edit-make-patch offset offset
                                  (format nil "~A~A" clock-line newline)
                                  heading))
               (let ((offset (edit-clock-drawer-insertion-offset document heading)))
                 (edit-make-patch
                  offset offset
                  (edit-clock-insertion-text document offset drawer-name clock-line)
                  heading))))
         (expected
           (edit-clock-in-expected document heading drawer-name clock-line
                                   (edit-patch-start patch)))
         (result (edit-apply-patches-result document (list patch)
                                            :target heading :expected expected)))
    (values result timestamp)))

(defun edit-clock-out-expected (document clock row)
  (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
         (path (edit-node-path document clock))
         (new-clock
           (edit-clock-canonical-fragment-node (format nil "~A~%" row) :clock))
         (properties (getf new-clock :properties)))
    (edit-canonical-set-property expected path :value (getf properties :value))
    (edit-canonical-set-property expected path :status (getf properties :status))
    expected))

(defun clock-out (document heading &key at time-context drawer)
  "Close HEADING's single running CLOCK at AT and return values edit-result and its interval.
TIME-CONTEXT is required and supplies the explicit zone and DST policy.
DRAWER is accepted for API symmetry and does not move an existing CLOCK."
  (declare (ignore drawer))
  (clock-check-heading document heading)
  (edit-clock-context-value at time-context)
  (clock-check-document-node-count document)
  (let* ((running (edit-clock-running-nodes heading))
         (clock (cond ((null running) (edit-clock-error :clock-not-running))
                      ((cdr running) (edit-clock-error :multiple-running-clocks))
                      (t (car running))))
         (start (nth-value 1 (clock-row-values clock)))
         (start-seconds (clock-context-instant-seconds start time-context))
         (end-seconds (clock-context-instant-seconds at time-context)))
    (when (< end-seconds start-seconds)
      (edit-clock-error :invalid-range
                        (format nil "~A ends before ~A"
                                (org-time-value-iso8601 at)
                                (org-time-value-iso8601 start))))
    (let* ((duration (floor (- end-seconds start-seconds) 60))
           (hours (floor duration 60))
           (minutes (mod duration 60))
           (source (document-text document))
           (timestamp-node (node-property clock :value))
           (timestamp-end (node-end timestamp-node))
           (start-timestamp
             (subseq source (node-begin timestamp-node) timestamp-end))
           (end-timestamp (edit-clock-timestamp at time-context))
           (duration-text (format nil "~D:~2,'0D" hours minutes))
           (interval (format nil "~A--~A => ~A"
                             start-timestamp end-timestamp duration-text))
           (clock-line (concatenate
                        'string
                        (subseq source (node-begin clock) timestamp-end)
                        "--" end-timestamp
                        (format nil " =>  ~A" duration-text)))
           (patch (multiple-value-bind (start end)
                      (edit-clock-clock-line-bounds clock)
                    (edit-make-patch start end clock-line heading)))
           (expected (edit-clock-out-expected document clock clock-line))
           (result (edit-apply-patches-result document (list patch)
                                              :target heading :expected expected)))
      (values result interval))))

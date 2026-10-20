;;;; src/edit-subtree-clone.lisp
;;;;
;;;; Format-preserving copies of headline subtrees with shifted timestamps.
(in-package #:org-kit)

(defun edit-subtree-clone-valid-shift-p (amount unit)
  (or (and (null amount) (null unit))
      (and (integerp amount)
           (member unit '(:hour :day :week :month :year)))))

(defun edit-subtree-clone-date-shift (date amount unit target)
  (handler-case
      (let ((shifted
              (case unit
                (:day (cl-date-kit:local-date-plus-days date amount))
                (:week (cl-date-kit:local-date-plus-days date (* amount 7)))
                (:month (cl-date-kit:local-date-plus-months date amount))
                (:year (cl-date-kit:local-date-plus-years date amount))
                (otherwise date))))
        (unless (<= 0 (cl-date-kit:local-date-year shifted) 9999)
          (edit-signal-error target :invalid-value
                             "Timestamp year is outside Org range"))
        shifted)
    (cl-date-kit:invalid-date ()
      (edit-signal-error target :invalid-value
                         "Timestamp date cannot be shifted by this amount"))))

(defun edit-subtree-clone-time-at (text position end)
  (multiple-value-bind (hour minute after)
      (%timestamp-time-at text position end)
    (when (and after
               (<= 0 hour 23)
               (<= 0 minute 59))
      (values hour minute after))))

(defun edit-subtree-clone-time-positions (text start end)
  (loop for position from start below end
        when (multiple-value-bind (hour minute after)
                 (edit-subtree-clone-time-at text position end)
               (declare (ignore hour minute))
               (and after (= after (+ position 5))))
          collect position))

(defun edit-subtree-clone-format-time (minutes)
  (format nil "~2,'0D:~2,'0D"
          (floor minutes 60)
          (mod minutes 60)))

(defun edit-subtree-clone-shift-hour (date hour minute amount target)
  (let* ((total (+ (* hour 60) minute (* amount 60)))
         (day-shift (floor total 1440))
         (time (mod total 1440)))
    (values (if (zerop day-shift)
                date
                (edit-subtree-clone-date-shift date day-shift :day target))
            time)))

(defun edit-subtree-clone-apply-local-patches (text patches)
  (with-output-to-string (stream)
    (loop with cursor = 0
          for (start end replacement) in
            (stable-sort (copy-list patches) #'< :key #'first)
          do (write-string text stream :start cursor :end start)
             (write-string replacement stream)
             (setf cursor end)
          finally (write-string text stream :start cursor))))
(defun edit-subtree-clone-apply-patches (document patches target)
  (let* ((prepared (edit-prepare-patches document patches :target target))
         (source
           (edit-splice-source
            (edit-prepared-patches-source prepared)
            (edit-prepared-patches-patches prepared))))
    (edit-reparse-source document source target)))

(defun edit-subtree-clone-timestamp-weekday-p (text start end)
  (member (subseq text start end)
          '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"
            "Monday" "Tuesday" "Wednesday" "Thursday"
            "Friday" "Saturday" "Sunday")
          :test #'string-equal))

(defun edit-subtree-clone-shift-timestamp-token (token amount unit target)
  (if (null amount)
      token
      (let* ((opening (char token 0))
             (closing (if (char= opening #\<) #\> #\]))
             (close (position closing token :from-end t))
             (content-end (or close (length token))))
        (multiple-value-bind (year month day date-end)
            (%timestamp-date-at token 1 content-end)
          (if (null date-end)
              token
              (handler-case
                  (let* ((date (cl-date-kit:make-local-date year month day))
                         (times (edit-subtree-clone-time-positions
                                 token date-end content-end))
                         (shifted-date date)
                         (shifted-times nil)
                         (patches nil))
                    (cond
                      ((eq unit :hour)
                       (if times
                           (progn
                             (multiple-value-bind (new-date new-time)
                                 (edit-subtree-clone-shift-hour
                                  date
                                  (nth-value 0 (%timestamp-time-at
                                                token (first times) content-end))
                                  (nth-value 1 (%timestamp-time-at
                                                token (first times) content-end))
                                  amount target)
                               (setf shifted-date new-date)
                               (push (cons (first times)
                                           (edit-subtree-clone-format-time new-time))
                                     shifted-times))
                             (dolist (position (rest times))
                               (multiple-value-bind (hour minute after)
                                   (%timestamp-time-at token position content-end)
                                 (declare (ignore after))
                                 (push (cons position
                                             (edit-subtree-clone-format-time
                                              (mod (+ (* hour 60) minute
                                                      (* amount 60))
                                                   1440)))
                                       shifted-times))))
                           (setf shifted-date
                                 (edit-subtree-clone-date-shift
                                  date (floor amount 24) :day target))))
                      (t
                       (setf shifted-date
                             (edit-subtree-clone-date-shift
                              date amount unit target))))
                    (unless (equalp date shifted-date)
                      (push (list 1 date-end
                                  (subseq (edit-todo-format-point-timestamp shifted-date) 0 10))
                            patches))
                    (dolist (entry shifted-times)
                      (push (list (car entry) (+ (car entry) 5) (cdr entry))
                            patches))
                    (let* ((weekday-start
                             (loop for position from date-end below content-end
                                   while (find (char token position)
                                               '(#\Space #\Tab))
                                   finally (return position)))
                           (weekday-end
                             (loop for position from weekday-start below content-end
                                   while (not (find (char token position)
                                                   '(#\Space #\Tab)))
                                   finally (return position))))
                      (if (and (< weekday-start weekday-end)
                               (edit-subtree-clone-timestamp-weekday-p
                                token weekday-start weekday-end))
                          (push (list weekday-start weekday-end
                                      (edit-todo-weekday-name shifted-date))
                                patches)
                          (push (list date-end weekday-start
                                      (if (< weekday-start content-end)
                                          (format nil " ~A "
                                                  (edit-todo-weekday-name shifted-date))
                                          (format nil " ~A"
                                                  (edit-todo-weekday-name shifted-date))))
                                patches)))
                    (edit-subtree-clone-apply-local-patches token patches))
                (cl-date-kit:invalid-date ()
                  (edit-signal-error target :invalid-value
                                     "Timestamp date cannot be shifted by this amount"))))))))

(defun edit-subtree-clone-adjustment-token-layouts (token)
  (let* ((opening (char token 0))
         (close (position (if (string= (string opening) "<")
                              (char ">" 0) (char "]" 0))
                          token :from-end t))
         (content-end (or close (length token)))
         (layouts nil))
    (multiple-value-bind (year month day date-end)
        (%timestamp-date-at token 1 content-end)
      (declare (ignore year month day))
      (when date-end
        (loop with position = date-end
              with previous-end = date-end
              while (< position content-end)
              do (let* ((token-start
                          (loop while (and (< position content-end)
                                           (%timestamp-whitespace-p
                                            (char token position)))
                                do (incf position)
                                finally (return position)))
                        (token-end
                          (loop for end from token-start below content-end
                                while (not (%timestamp-whitespace-p
                                            (char token end)))
                                finally (return end))))
                   (when (= token-start content-end)
                     (return))
                   (multiple-value-bind (kind mode amount unit valid-p)
                       (%timestamp-adjustment-token
                        token token-start token-end)
                     (declare (ignore mode amount unit))
                     (when valid-p
                       (push (list kind previous-end token-start token-end)
                             layouts)))
                   (setf previous-end token-end
                         position token-end))))
    (nreverse layouts))))
(defun edit-subtree-clone-remove-repeater-token (token)
  (let ((layout
          (find :repeater
                (edit-subtree-clone-adjustment-token-layouts token)
                :key (lambda (entry) (first entry)))))
    (if layout
        (format nil "~A~A"
                (subseq token 0 (second layout))
                (subseq token (fourth layout)))
        token)))

(defun edit-subtree-clone-timestamp-spans (source)
  (loop with position = 0
        while (< position (length source))
        for opening = (position-if (lambda (character)
                                     (find character '(#\< #\[)))
                                   source :start position)
        while opening
        for closing-character = (if (char= (char source opening) #\<)
                                    #\> #\])
        for closing = (position closing-character source :start (1+ opening))
        for valid-p =
          (and closing
               (nth-value 3 (%timestamp-date-at source (1+ opening) closing)))
        when valid-p
          collect (list opening (1+ closing) (char source opening))
        do (setf position (if closing (1+ closing) (1+ opening)))))

(defun edit-subtree-clone-token-has-repeater-p (token)
  (and (string= (string (char token 0)) "<")
       (find :repeater
             (edit-subtree-clone-adjustment-token-layouts token)
             :key (lambda (layout) (first layout)))))

(defun edit-subtree-clone-active-repeater-p (source)
  (some (lambda (span)
          (let* ((start (first span))
                 (end (second span))
                 (token (subseq source start end)))
            (edit-subtree-clone-token-has-repeater-p token)))
        (edit-subtree-clone-timestamp-spans source)))

(defun edit-subtree-clone-transform-timestamps (document amount unit
                                                remove-repeaters-p target)
  (let* ((source (document-text document))
         (patches
           (loop for (start end opening) in
                 (edit-subtree-clone-timestamp-spans source)
                 for token = (subseq source start end)
                 for active-p = (char= opening #\<)
                 for transformed =
                   (let ((shifted
                           (edit-subtree-clone-shift-timestamp-token
                            token amount unit target)))
                     (if (and active-p remove-repeaters-p)
                         (edit-subtree-clone-remove-repeater-token shifted)
                         shifted))
                 unless (string= token transformed)
                   collect (edit-make-patch start end transformed target))))
    (if patches
        (edit-subtree-clone-apply-patches document patches target)
        document)))

(defun edit-subtree-clone-clock-line-p (source start end)
  (let ((position start))
    (loop while (and (< position end)
                     (find (char source position) '(#\Space #\Tab)))
          do (incf position))
    (and (<= (+ position 6) end)
         (string= source "CLOCK:" :start1 position :end1 (+ position 6)))))

(defun edit-subtree-clone-remove-clocks (document target)
  (let* ((source (document-text document))
         (patches
           (loop with start = 0
                 while (< start (length source))
                 for newline = (position #\Newline source :start start)
                 for line-end = (or newline (length source))
                 for next = (if newline (1+ newline) line-end)
                 when (edit-subtree-clone-clock-line-p source start line-end)
                   collect (edit-make-patch start next "" target)
                 do (setf start next))))
    (if patches
        (edit-subtree-clone-apply-patches document patches target)
        document)))

(defun edit-subtree-clone-empty-drawer-p (source drawer)
  (let ((start (node-contents-begin drawer))
        (end (node-contents-end drawer)))
    (or (null start)
        (every (lambda (character)
                 (find character '(#\Space #\Tab #\Newline #\Return)))
               (subseq source start end)))))

(defun edit-subtree-clone-remove-empty-drawers (document target)
  (let ((source (document-text document))
        (patches nil))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :drawer)
                  (edit-subtree-clone-empty-drawer-p source node))
         (push (edit-make-patch (node-begin node) (node-end node) "" target)
               patches)))
     (document-root document))
    (if patches
        (edit-subtree-clone-apply-patches document patches target)
        document)))

(defun edit-subtree-clone-id-set (document)
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (entry (org-id-document-identifiers document :reject-duplicates t))
      (setf (gethash (car entry) ids) t))
    ids))

(defun edit-subtree-clone-copy (document raw-subtree amount unit
                                remove-clocks-p remove-repeaters-p
                                root-id-p ids random-source
                                random-source-supplied-p target)
  (multiple-value-bind (fragment headline)
      (edit-subtree-fragment document raw-subtree target)
    (when root-id-p
      (multiple-value-bind (updated id)
          (if random-source-supplied-p
              (set-heading-id fragment headline :random-source random-source)
              (set-heading-id fragment headline))
        (when (gethash id ids)
          (org-id-signal-error :duplicate-id
                               "Generated ID already occurs in the document"))
        (setf (gethash id ids) t
              fragment updated)))
    (when remove-clocks-p
      (setf fragment (edit-subtree-clone-remove-clocks fragment target)
            fragment (edit-subtree-clone-remove-empty-drawers fragment target)))
    (setf fragment
          (edit-subtree-clone-transform-timestamps
           fragment amount unit remove-repeaters-p target))
    (list (document-text fragment)
          (first (getf (edit-canonical-document fragment) :children)))))

(defun edit-subtree-clone-final-newline (text newline)
  (if (and (plusp (length text))
           (char= (char text (1- (length text))) #\Newline))
      text
      (concatenate 'string text newline)))

(defun clone-subtree-with-time-shift (document target count shift-amount
                                               shift-unit &key
                                               (random-source nil
                                                              random-source-supplied-p))
  "Clone TARGET as siblings with shifted timestamps and return an EDIT-RESULT."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (typep count '(integer 0))
    (edit-signal-error target :invalid-value
                       "Clone count must be a non-negative integer"))
  (unless (edit-subtree-clone-valid-shift-p shift-amount shift-unit)
    (edit-signal-error target :invalid-value
                       "Shift amount must be NIL with no unit, or an integer and valid unit"))
  (let* ((headline (edit-resolve-node document target '(:headline)))
         (source (document-text document))
         (root-id-p (not (null (org-id-target-id-properties headline))))
         (explicit-shift-p (not (null shift-amount)))
         (newline (edit-subtree-line-ending document (node-begin headline))))
    (multiple-value-bind (start end)
        (edit-subtree-safe-span document headline target)
      (let* ((raw-subtree (subseq source start end))
             (repeater-p (and explicit-shift-p
                              (edit-subtree-clone-active-repeater-p raw-subtree)))
             (clone-count (if repeater-p (+ count 2) count))
             (ids (progn
                    (check-limit (document-limits document)
                                 :max-subtree-clones clone-count)
                    (and root-id-p (plusp clone-count)
                         (edit-subtree-clone-id-set document))))
             (trailing-headline-cr-p
              (edit-subtree-trailing-headline-cr-p document start end))
             (first-index (if repeater-p 0 1))
             (last-index (if repeater-p (1+ count) count))
             (copies
              (loop for index from first-index to last-index
                    for remove-clocks-p =
                      (if repeater-p (not (zerop index)) t)
                    for remove-repeaters-p =
                      (and explicit-shift-p
                           (or (not repeater-p) (/= index last-index)))
                    collect
                      (edit-subtree-clone-copy
                       document raw-subtree
                       (and explicit-shift-p (* index shift-amount))
                       shift-unit remove-clocks-p remove-repeaters-p
                       root-id-p ids random-source random-source-supplied-p
                       target)))
             (combined
              (format nil "~{~A~}"
                      (mapcar (lambda (copy)
                                (edit-subtree-clone-final-newline
                                 (first copy) newline))
                              copies)))
             (expected
              (let* ((canonical
                       (edit-canonical-copy (edit-canonical-document document)))
                     (path (edit-node-path document headline target))
                     (parent-path (butlast path))
                     (index (car (last path)))
                     (children
                       (edit-subtree-canonical-children canonical parent-path))
                     (copy-nodes
                       (mapcar (lambda (copy) (second copy)) copies)))
                (edit-canonical-set-children
                 canonical parent-path
                 (if repeater-p
                     (append (subseq children 0 index)
                             copy-nodes
                             (nthcdr (1+ index) children))
                     (append (subseq children 0 (1+ index))
                             copy-nodes
                             (nthcdr (1+ index) children))))
                canonical)))
        (if repeater-p
            (let* ((clone-text (edit-subtree-clone-final-newline combined newline))
                   (suffix-p (or (< end (length source))
                                 (document-final-newline-p document)))
                   (replacement
                    (edit-subtree-join-fragment
                     document start clone-text suffix-p newline
                     trailing-headline-cr-p)))
              (edit-apply-patches-result
               document (list (edit-make-patch start end replacement target))
               :target target
               :expected expected))
          (let ((clone-text (if copies
                                (edit-subtree-clone-final-newline combined newline)
                              "")))
            (cond
             (copies
              (let* ((suffix-p (or (< end (length source))
                                   (document-final-newline-p document)))
                     (replacement
                      (edit-subtree-join-fragment
                       document end clone-text suffix-p newline
                       trailing-headline-cr-p)))
                (edit-apply-patches-result
                 document
                 (list (edit-make-patch end end replacement target))
                 :target target
                 :expected expected)))
             (t (make-edit-result :updated-document document :spans nil)))))))))

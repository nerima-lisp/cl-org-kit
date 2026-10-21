;;;; src/agenda-bulk.lisp
;;;; Pure, staged edits over agenda selections.
(in-package #:org-kit)

(defstruct (agenda-bulk-selection
            (:constructor make-agenda-bulk-selection
                (document heading path position))
            (:copier nil))
  document heading path position)

(defun agenda-bulk-signal-invalid (context message)
  (agenda-signal-error :invalid-option context message))

(defun agenda-bulk-entry-heading (entry)
  (cond
    ((agenda-match-entry-p entry)
     (agenda-match-entry-node entry))
    ((agenda-entry-p entry)
     (loop for node = (agenda-entry-source-node entry) then (node-parent node)
           while node
           when (eq (node-type node) :headline)
             return node))))

(defun agenda-bulk-view-entries (view)
  (let ((length (and (listp view) (agenda-proper-list-length view))))
    (if (and (integerp length)
             (every (lambda (entry)
                      (or (agenda-entry-p entry)
                          (agenda-match-entry-p entry)))
                    view))
        view
        (agenda-write-result-entries view))))

(defun agenda-bulk-selections (entries)
  (let ((selections nil)
        (seen (make-hash-table :test #'eq)))
    (dolist (entry entries)
      (let* ((document (agenda-write-entry-document entry))
             (heading (agenda-bulk-entry-heading entry)))
        (unless (and (document-p document)
                     (node-p heading)
                     (eq (node-type heading) :headline)
                     (eq (node-document heading) document))
          (agenda-bulk-signal-invalid
           entry "Bulk actions require agenda entries with source headlines"))
        (let ((document-seen
                (or (gethash document seen)
                    (setf (gethash document seen)
                          (make-hash-table :test #'eq)))))
          (unless (gethash heading document-seen)
            (setf (gethash heading document-seen) t)
            (push (make-agenda-bulk-selection
                   document heading (node-heading-path heading)
                   (node-begin heading))
                  selections)))))
    (nreverse selections)))

(defun agenda-bulk-unique-documents (documents)
  (let ((seen (make-hash-table :test #'eq))
        (unique nil))
    (dolist (document documents (nreverse unique))
      (unless (gethash document seen)
        (setf (gethash document seen) t)
        (push document unique)))))

(defun agenda-bulk-selected-documents (selections &optional target-document)
  (agenda-bulk-unique-documents
   (append (mapcar #'agenda-bulk-selection-document selections)
           (when target-document (list target-document)))))

(defun agenda-bulk-check-limit (entries &optional target-document)
  (let* ((documents
           (agenda-bulk-unique-documents
            (append (remove-if-not
                     #'document-p
                     (mapcar #'agenda-write-entry-document entries))
                    (when target-document (list target-document)))))
         (limits (mapcar #'document-limits documents)))
    (check-limit
     (make-limits :max-agenda-bulk-actions
                  (minimum-limit-value limits :max-agenda-bulk-actions))
     :max-agenda-bulk-actions (length entries))))

(defun agenda-bulk-ancestor-p (ancestor node)
  (loop for current = (node-parent node) then (node-parent current)
        while current
        thereis (eq current ancestor)))

(defun agenda-bulk-top-level-selections (selections)
  (remove-if
   (lambda (selection)
     (some (lambda (other)
             (and (not (eq selection other))
                  (eq (agenda-bulk-selection-document selection)
                      (agenda-bulk-selection-document other))
                  (agenda-bulk-ancestor-p
                   (agenda-bulk-selection-heading other)
                   (agenda-bulk-selection-heading selection))))
           selections))
   selections))

(defun agenda-bulk-order-selections (selections action)
  (let* ((structural-p (member action '(:refile :archive)))
         (selected (if structural-p
                       (agenda-bulk-top-level-selections selections)
                       selections))
         (documents (agenda-bulk-selected-documents selected)))
    (loop for document in documents
          append (stable-sort
                  (remove-if-not
                   (lambda (selection)
                     (eq (agenda-bulk-selection-document selection) document))
                   (copy-list selected))
                  #'<
                  :key #'agenda-bulk-selection-position))))
(defun agenda-bulk-adjust-path-for-selections
    (path document processed-selections)
  (let ((adjusted-path (copy-tree path)))
    (loop for segment in path
          for index from 0
          for removed-count =
            (count-if
             (lambda (selection)
               (let ((candidate-path
                       (agenda-bulk-selection-path selection)))
                 (and (eq (agenda-bulk-selection-document selection) document)
                      (= (length candidate-path) (1+ index))
                      (equal (subseq candidate-path 0 index)
                             (subseq path 0 index))
                      (equal (getf (nth index candidate-path) :title)
                             (getf segment :title)))))
             processed-selections)
          when (plusp removed-count)
            do (decf (getf (nth index adjusted-path) :index)
                     removed-count))
    adjusted-path))

(defun agenda-bulk-resolve-heading-path (document path)
  (loop with parent = (document-root document)
        for segment in path
        for matches = (remove-if-not
                       (lambda (child)
                         (and (eq (node-type child) :headline)
                              (string= (node-property child :raw-value "")
                                       (getf segment :title))))
                       (node-children parent))
        for index = (getf segment :index)
        for heading = (and (integerp index) (plusp index)
                           (nth (1- index) matches))
        do (unless heading
             (agenda-signal-error :target-not-found path
                                  "Selected agenda headline no longer exists"))
           (setf parent heading)
        finally (return parent)))

(defun agenda-bulk-stage-documents (documents)
  (mapcar (lambda (document) (cons document document)) documents))

(defun agenda-bulk-current-document (staged original)
  (let ((entry (assoc original staged :test #'eq)))
    (unless entry
      (agenda-signal-error :target-not-found original
                           "Bulk action document was not staged"))
    (cdr entry)))

(defun agenda-bulk-stage-document (staged original updated)
  (let ((entry (assoc original staged :test #'eq)))
    (unless entry
      (agenda-signal-error :target-not-found original
                           "Bulk action document was not staged"))
    (setf (cdr entry) updated)
    staged))
(defun agenda-bulk-archive-target-document
    (source-document target-document original-target-document)
  (if (eq source-document original-target-document)
      target-document
      (let* ((source-name (document-source-name source-document))
             (source (document-text target-document))
             (original-target-empty-p
               (string= (document-text original-target-document) ""))
             (empty-target-p (string= source ""))
             (newline (edit-subtree-line-ending target-document (length source)))
             (archive-header
               (if (and original-target-empty-p empty-target-p
                        (stringp source-name))
                   (format nil "Archived entries from file ~A~A~A~A"
                           source-name newline newline newline)
                   ""))
             (header
               (if (string= archive-header "")
                   ""
                   (format nil "#    -*- mode: org -*-~A~A~A~A"
                           newline newline newline archive-header)))
             (separator (if empty-target-p "" newline))
             (insertion (concatenate 'string header separator)))
        (if (string= insertion "")
            target-document
            (let* ((updated-source (concatenate 'string source insertion))
                   (expected
                     (edit-canonical-document
                      (edit-reparse-source target-document updated-source
                                           :archive-header))))
              (edit-result-updated-document
               (edit-apply-patches-result
                target-document
                (list (edit-make-patch (length source) (length source) insertion
                                       :archive-header))
                :target :archive-header :expected expected)))))))
(defun agenda-bulk-align-heading-tags (document path)
  (let ((heading (agenda-bulk-resolve-heading-path document path)))
    (multiple-value-bind (tag-start tag-end)
        (edit-headline-tag-span document heading)
      (if (null tag-start)
          document
          (let* ((source (document-text document))
                 (line-start (node-begin heading))
                 (blank-start
                   (1+ (position-if-not
                        (lambda (character)
                          (or (char= character #\Space)
                              (char= character #\Tab)))
                        source :start line-start :end tag-start :from-end t)))
                 (prefix (subseq source line-start blank-start))
                 (tag-string (subseq source tag-start tag-end))
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
                 (replacement
                   (make-string (- tag-column prefix-width)
                                :initial-element #\Space))
                 (result
                   (edit-apply-patches-result
                    document
                    (list (edit-make-patch blank-start tag-start replacement
                                           heading))
                    :target heading
                    :expected (edit-canonical-document document))))
            (edit-result-updated-document result))))))
(defun agenda-bulk-align-moved-subtree-tags (document path)
  (let ((heading (agenda-bulk-resolve-heading-path document path))
        (paths nil))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :headline)
         (push (node-heading-path node) paths)))
     heading)
    (dolist (heading-path (nreverse paths) document)
      (setf document
            (agenda-bulk-align-heading-tags document heading-path)))))

(defun agenda-bulk-date (value)
  (let ((date
          (cond
            ((cl-date-kit:local-date-p value) value)
            ((org-time-value-p value)
             (unless (eq (org-time-value-kind value) :date)
               (agenda-bulk-signal-invalid
                value "Schedule dates must be date-only values"))
             (cl-date-kit:make-local-date
              (org-time-value-year value)
              (org-time-value-month value)
              (org-time-value-day value)))
            ((stringp value)
             (let ((time (handler-case (make-org-time-value value)
                           (org-time-error () nil))))
               (unless (and time (eq (org-time-value-kind time) :date))
                 (agenda-bulk-signal-invalid
                  value "Schedule dates must use YYYY-MM-DD"))
               (cl-date-kit:make-local-date
                (org-time-value-year time)
                (org-time-value-month time)
                (org-time-value-day time)))) (t
             (agenda-bulk-signal-invalid
              value "Schedule dates must be date-only values")))))
    date))

(defun agenda-bulk-time-value (date)
  (make-org-time-value
   (format nil "~4,'0D-~2,'0D-~2,'0D"
           (cl-date-kit:local-date-year date)
           (cl-date-kit:local-date-month date)
           (cl-date-kit:local-date-day date))))

(defun agenda-bulk-timestamp-date (timestamp endpoint)
  (cl-date-kit:make-local-date
   (node-property timestamp (if (eq endpoint :start) :year-start :year-end))
   (node-property timestamp (if (eq endpoint :start) :month-start :month-end))
   (node-property timestamp (if (eq endpoint :start) :day-start :day-end))))

(defun agenda-bulk-unparsed-scheduled-paragraph (document heading)
  (let* ((source (document-text document))
         (section (edit-properties-headline-section heading)))
    (find-if
     (lambda (paragraph)
       (and (eq (node-type paragraph) :paragraph)
            (find-if
             (lambda (timestamp)
               (and (eq (node-type timestamp) :timestamp)
                    (let* ((paragraph-start (node-begin paragraph))
                           (timestamp-start (node-begin timestamp))
                           (keyword-end (+ paragraph-start
                                           (length "SCHEDULED:"))))
                      (and (<= keyword-end timestamp-start)
                           (string-equal "SCHEDULED:" source
                                         :start2 paragraph-start
                                         :end2 keyword-end)
                           (let ((value-start
                                   (reader-heading-skip-horizontal-space
                                    source keyword-end timestamp-start)))
                             (and (< keyword-end value-start)
                                  (= value-start timestamp-start)))))))
             (node-children paragraph))))
     (and section (node-children section)))))

(defun agenda-bulk-remove-scheduled-paragraph (document heading paragraph)
  (let* ((source (document-text document))
         (timestamp
           (find :timestamp
                 (node-children paragraph)
                 :key (function node-type)))
         (timestamp-end (node-end timestamp))
         (line-end
           (position #\Newline source
                     :start timestamp-end
                     :end (node-end paragraph)))
         (patch-end
           (if (and line-end (= line-end timestamp-end))
               (1+ line-end)
               timestamp-end))
         (patch (edit-make-patch (node-begin paragraph) patch-end
                                 "" heading))
         (updated-source (edit-splice-source source (list patch)))
         (expected
           (edit-canonical-document
            (edit-reparse-source document updated-source heading))))
    (edit-apply-patches-result document (list patch) :target heading
                               :expected expected)))

(defun agenda-bulk-schedule-edit (document heading date)
  (let* ((section (edit-properties-headline-section heading))
         (planning (edit-properties-section-planning section))
         (unparsed-scheduled-paragraph
           (and (null planning)
                (agenda-bulk-unparsed-scheduled-paragraph document heading)))
         (unparsed-scheduled-timestamp
           (and unparsed-scheduled-paragraph
                (find :timestamp (node-children unparsed-scheduled-paragraph)
                      :key #'node-type)))
         (timestamp (or (and planning (node-property planning :scheduled))
                        unparsed-scheduled-timestamp)))
    (cond
      ((null date)
       (if timestamp
           (if unparsed-scheduled-paragraph
               (agenda-bulk-remove-scheduled-paragraph
                document heading unparsed-scheduled-paragraph)
               (remove-planning document heading :scheduled))
           (make-edit-result :updated-document document :spans nil)))
      ((null timestamp)
       (let* ((value (agenda-bulk-time-value date))
              (text (format nil "<~A>" (edit-todo-format-point-timestamp value))))
         (set-planning document heading :scheduled text)))
      (t
       (let* ((source (document-text document))
              (ranges (edit-timestamp-endpoint-ranges document timestamp heading))
              (range-count (length ranges))
              (timestamp-start (node-begin timestamp))
              (old-start (agenda-bulk-timestamp-date timestamp :start))
              (delta (- (cl-date-kit:local-date-to-epoch-day date)
                        (cl-date-kit:local-date-to-epoch-day old-start)))
              (updated-end (and (= range-count 2)
                                (cl-date-kit:local-date-plus-days
                                 (agenda-bulk-timestamp-date timestamp :end)
                                 delta)))
              (patches
                (append
                 (edit-timestamp-date-patches
                  document (first ranges) date heading)
                 (when updated-end
                   (edit-timestamp-date-patches
                    document (second ranges) updated-end heading))))
              (timestamp-patches
                (mapcar (lambda (patch)
                          (edit-make-patch
                           (- (edit-patch-start patch) timestamp-start)
                           (- (edit-patch-end patch) timestamp-start)
                           (edit-patch-replacement patch) heading))
                        patches))
              (timestamp-text
                (edit-splice-source
                 (subseq source timestamp-start (node-end timestamp))
                 timestamp-patches))
              (updated-source (and unparsed-scheduled-paragraph
                                   (edit-splice-source source patches))))
         (if unparsed-scheduled-paragraph
             (edit-apply-patches-result
              document patches :target heading
              :expected
              (edit-canonical-document
               (edit-reparse-source document updated-source heading)))
             (set-planning document heading :scheduled timestamp-text)))))))

(defun agenda-bulk-target-path (target-document target-heading)
  (when target-heading
    (unless (document-p target-document)
      (agenda-bulk-signal-invalid target-document
                                  "A target document is required for a target heading"))
    (node-heading-path
     (edit-resolve-node target-document target-heading '(:headline)))))

(defun agenda-bulk-target-node (document path)
  (when path
    (agenda-bulk-resolve-heading-path document path)))

(defun agenda-bulk-check-target (selections target-document target-heading)
  (when target-heading
    (dolist (selection selections)
      (when (and (eq (agenda-bulk-selection-document selection) target-document)
                 (let ((heading (agenda-bulk-selection-heading selection)))
                   (or (eq heading target-heading)
                       (agenda-bulk-ancestor-p heading target-heading))))
        (agenda-bulk-signal-invalid
         target-heading "A bulk action cannot move a heading into its own subtree")))))

(defun agenda-bulk-edit-entry
    (staged source-original-document document heading action state at tag
     schedule-date target-document target-heading-path time-context)
  (case action
    (:todo
     (transition-heading-todo
      document heading state :at at :time-context time-context
      :workflow-options (make-org-workflow-options :log-done nil :log-repeat nil)
      :inhibit-blocking t))
    (:add-tag
     (let* ((path (node-heading-path heading))
            (updated (add-heading-tags document heading (list tag))))
       (make-edit-result
        :updated-document (agenda-bulk-align-heading-tags updated path))))
    (:schedule
     (let ((result (agenda-bulk-schedule-edit document heading schedule-date)))
       (if (edit-result-p result)
           result
           (make-edit-result :updated-document result :spans nil))))
    (:refile
     (let* ((current-target (agenda-bulk-current-document
                             staged target-document))
            (target-heading
              (agenda-bulk-target-node current-target target-heading-path)))
       (refile-subtree document heading current-target target-heading)))
    (:archive
     (if target-document
         (let* ((timestamp (archive-time-string time-context))
                (tagged-document
                  (set-property document heading "ARCHIVE_TIME" timestamp))
                (current-heading
                  (agenda-bulk-resolve-heading-path
                   tagged-document (node-heading-path heading)))
                (current-target
                  (agenda-bulk-archive-target-document
                   source-original-document
                   (if (eq source-original-document target-document)
                       tagged-document
                       (agenda-bulk-current-document staged target-document))
                   target-document))
                (target-heading
                  (agenda-bulk-target-node current-target target-heading-path)))
           (archive-subtree-to tagged-document current-heading current-target
                               :target-heading target-heading))
         (archive-to-sibling document heading :time-context time-context)))))

(defun agenda-bulk-edit-result (original updated)
  (let* ((before (document-text original))
         (after (document-text updated))
         (prefix (loop for index below (min (length before) (length after))
                       while (char= (char before index) (char after index))
                       finally (return index)))
         (old-end (length before))
         (new-end (length after)))
    (loop while (and (> old-end prefix)
                     (> new-end prefix)
                     (char= (char before (1- old-end))
                            (char after (1- new-end)))) do (decf old-end) (decf new-end))
    (make-edit-result
     :updated-document updated
     :spans (unless (and (= prefix old-end) (= prefix new-end))
              (list (make-edit-span prefix old-end))))))

(defun agenda-bulk-results (staged)
  (mapcar (lambda (entry)
            (cons (car entry)
                  (agenda-bulk-edit-result (car entry) (cdr entry))))
          staged))

(defun agenda-bulk-check-output-limit (original updated)
  (check-limit
   (document-limits original)
   :max-output-octets
   (edit-prospective-output-octets updated (document-text updated) nil)))

(defun agenda-bulk-validate-action
    (action state state-supplied-p at-supplied-p tag tag-supplied-p
     schedule-date-supplied-p target-heading target-document time-context)
  (unless (member action '(:todo :add-tag :schedule :refile :archive))
    (agenda-bulk-signal-invalid action "Unknown agenda bulk action"))
  (when (and target-heading (not (member action '(:refile :archive))))
    (agenda-bulk-signal-invalid
     target-heading ":TARGET-HEADING is only valid for :REFILE or :ARCHIVE"))
  (case action
    (:todo
     (unless (and state-supplied-p at-supplied-p)
       (agenda-bulk-signal-invalid action
                                   "TODO bulk actions require :STATE and :AT"))
     (unless (or (null state) (stringp state))
       (agenda-bulk-signal-invalid state "TODO state must be a string or NIL")))
    (:add-tag
     (unless (and tag-supplied-p (stringp tag))
       (agenda-bulk-signal-invalid tag "Tag bulk actions require a string :TAG")))
    (:schedule
     (unless schedule-date-supplied-p
       (agenda-bulk-signal-invalid action
                                   "Schedule bulk actions require :SCHEDULE-DATE")))
    (:refile
     (unless (document-p target-document)
       (agenda-bulk-signal-invalid target-document
                                   "Refile bulk actions require :TARGET-DOCUMENT")))
    (:archive
     (unless (org-time-context-p time-context)
       (agenda-bulk-signal-invalid time-context
                                   "Archive bulk actions require :TIME-CONTEXT"))
     (when (and target-document (not (document-p target-document)))
       (agenda-bulk-signal-invalid target-document
                                   "Archive target must be a document")))))

(defun agenda-bulk-action
    (view action &key (state nil state-supplied-p)
                     (at nil at-supplied-p)
                     (tag nil tag-supplied-p)
                     (schedule-date nil schedule-date-supplied-p)
                     target-document target-heading time-context)
  "Apply ACTION to agenda entries and return document/edit-result pairs.

VIEW is an agenda result, custom view, or list of agenda entries. ACTION is
:TODO, :ADD-TAG, :SCHEDULE, :REFILE, or :ARCHIVE. Schedule dates accept an
ORG-TIME-VALUE, local date, ISO date string, or NIL to remove SCHEDULED. Refile
and archive destinations are supplied as documents; an archive without a
target document moves entries below an Archive sibling. The function stages
all changes in memory and never writes files. Each result is a pair whose CAR
is the original document and whose CDR is its combined EDIT-RESULT."
  (agenda-bulk-validate-action action state state-supplied-p
                                at-supplied-p tag tag-supplied-p
                                schedule-date-supplied-p target-heading
                                target-document time-context)
  (let ((entries (agenda-bulk-view-entries view)))
    (agenda-bulk-check-limit entries target-document)
    (let* ((selections (agenda-bulk-selections entries))
           (schedule-date (when (and (eq action :schedule) schedule-date)
                            (agenda-bulk-date schedule-date)))
           (target-heading-path
             (agenda-bulk-target-path target-document target-heading)))
      (unless selections
        (return-from agenda-bulk-action nil))
      (when (member action '(:refile :archive))
        (agenda-bulk-check-target selections target-document target-heading))
      (let* ((documents (agenda-bulk-selected-documents selections target-document))
             (staged (agenda-bulk-stage-documents documents))
             (ordered-selections (agenda-bulk-order-selections selections action))
             (processed-selections nil))
        (let ((current-selection nil))
          (handler-case
              (progn
                (dolist (selection ordered-selections)
                  (setf current-selection selection)
                  (let* ((source-document
                           (agenda-bulk-selection-document selection))
                         (current-document
                           (agenda-bulk-current-document staged source-document))
                         (selection-path
                           (if (member action '(:refile :archive))
                               (agenda-bulk-adjust-path-for-selections
                                (agenda-bulk-selection-path selection)
                                source-document processed-selections)
                               (agenda-bulk-selection-path selection)))
                         (current-target-heading-path
                           (if (and target-heading-path
                                    (member action '(:refile :archive))
                                    (member target-document documents :test #'eq))
                               (agenda-bulk-adjust-path-for-selections
                                target-heading-path target-document
                                processed-selections)
                               target-heading-path))
                         (heading
                           (agenda-bulk-resolve-heading-path
                            current-document selection-path))
                         (result
                           (agenda-bulk-edit-entry
                            staged source-document current-document heading action
                            state at tag schedule-date target-document
                            current-target-heading-path time-context)))
                    (cond
                      ((member action '(:todo :add-tag :schedule))
                       (let ((updated (edit-result-updated-document result)))
                         (agenda-bulk-check-output-limit source-document updated)
                         (setf staged
                               (agenda-bulk-stage-document staged source-document
                                                           updated))))
                      ((and (eq action :archive) (null target-document))
                       (let ((updated (edit-result-updated-document result)))
                         (agenda-bulk-check-output-limit source-document updated)
                         (setf staged
                               (agenda-bulk-stage-document staged source-document
                                                           updated))))
                      (t
                       (let* ((source-edit
                                (relocation-result-source-edit result))
                              (target-edit
                                (relocation-result-target-edit result))
                              (source-updated
                                (edit-result-updated-document source-edit))
                              (target-updated
                                (if (eq action :refile)
                                    (agenda-bulk-align-moved-subtree-tags
                                     (edit-result-updated-document target-edit)
                                     (relocation-result-target-path result))
                                    (edit-result-updated-document target-edit)))
                              (same-document-p (eq source-document target-document)))
                         (if same-document-p
                             (agenda-bulk-check-output-limit source-document
                                                             target-updated)
                             (progn
                               (agenda-bulk-check-output-limit source-document
                                                               source-updated)
                               (agenda-bulk-check-output-limit target-document
                                                               target-updated)))
                         (setf staged
                               (agenda-bulk-stage-document
                                staged source-document
                                (if same-document-p target-updated source-updated)))
                         (when (and target-document (not same-document-p))
                           (setf staged
                                 (agenda-bulk-stage-document staged target-document
                                                             target-updated))))))
                    (push selection processed-selections)))
                (agenda-bulk-results staged))
            (org-kit-error (condition)
              (agenda-signal-error
               :bulk-action-failed
               (format nil "document ~D selection at ~D: ~{~A~^/~}"
                       (1+ (position
                            (agenda-bulk-selection-document current-selection)
                            documents :test #'eq))
                       (agenda-bulk-selection-position current-selection)
                       (mapcar
                        (lambda (segment)
                          (format nil "~D ~A" (getf segment :index)
                                  (getf segment :title)))
                        (agenda-bulk-selection-path current-selection)))
               (format nil "Bulk action failed: ~A" condition)))))))))

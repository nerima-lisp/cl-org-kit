;;;; src/edit-headline.lisp
;;;;
;;;; Format-preserving edits to headline fields.
(in-package #:org-kit)

(defun edit-headline-check-document (document)
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document)))

(defun edit-headline-resolve-target (document target)
  (edit-headline-check-document document)
  (edit-resolve-node document target '(:headline)))

(defun edit-headline-line-end (document headline)
  (let* ((source (document-text document))
         (start (node-begin headline))
         (newline (position #\Newline source :start start)))
    (if (and newline
             (> newline start)
             (char= (char source (1- newline)) #\Return))
        (1- newline)
        (or newline (length source)))))

(defun edit-headline-marker-end (document headline)
  (let* ((source (document-text document))
         (start (node-begin headline))
         (line-end (edit-headline-line-end document headline)))
    (loop for position from start below line-end
          while (char= (char source position) #\*)
          finally (return position))))

(defun edit-headline-skip-space (source position end)
  (loop while (and (< position end)
                   (reader-heading-horizontal-space-p (char source position)))
        do (incf position)
        finally (return position)))

(defun edit-headline-prefix-layout (document headline)
  (let* ((source (document-text document))
         (line-end (edit-headline-line-end document headline))
         (marker-end (edit-headline-marker-end document headline))
         (position (edit-headline-skip-space source marker-end line-end))
         (todo-keyword (node-property headline :todo-keyword))
         (todo-start nil)
         (todo-end nil)
         (priority-start nil)
         (priority-end nil))
    (when todo-keyword
      (setf todo-start position
            todo-end (+ position (length todo-keyword)))
      (setf position (edit-headline-skip-space source todo-end line-end)))
    (let ((priority (node-property headline :priority)))
      (when priority
        (setf priority-start position
              priority-end
              (nth-value 1 (reader-heading-priority-at source position line-end)))))
    (values marker-end todo-start todo-end priority-start priority-end)))

(defun edit-headline-tag-span (document headline)
  (let* ((source (document-text document))
         (marker-end (edit-headline-marker-end document headline))
         (line-end (edit-headline-line-end document headline)))
    (let ((tag-start
            (nth-value 1 (reader-heading-tags-at-end source marker-end line-end))))
      (when tag-start
        (values tag-start
                (reader-heading-trim-right source marker-end line-end))))))

(defun edit-headline-title-span (document headline)
  (let* ((source (document-text document))
         (line-end (edit-headline-line-end document headline)))
    (multiple-value-bind (marker-end todo-start todo-end
                          priority-start priority-end)
        (edit-headline-prefix-layout document headline)
      (declare (ignore todo-start priority-start))
      (let ((position (edit-headline-skip-space
                       source (or priority-end todo-end marker-end) line-end)))
        (when (node-property headline :commentedp)
          (setf position
                (edit-headline-skip-space source (+ position (length "COMMENT"))
                                          line-end)))
        (multiple-value-bind (tag-start tag-end)
            (edit-headline-tag-span document headline)
          (declare (ignore tag-end))
          (let* ((end (reader-heading-trim-right source position
                                                 (or tag-start line-end)))
                 (start (edit-headline-skip-space source position end)))
            (values start end)))))))

(defun edit-headline-expected (document headline changes)
  (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
         (path (edit-node-path document headline)))
    (loop for (key value) on changes by #'cddr
          do (edit-canonical-set-property expected path key value))
    expected))

(defun edit-headline-apply (document headline target patches changes)
  (edit-apply-patches document patches
                      :target target
                      :expected (edit-headline-expected document headline changes)))

(defun edit-headline-todo-entry (document keyword target)
  (when keyword
    (unless (stringp keyword)
      (edit-signal-error target :invalid-value
                         "TODO keyword must be a configured string"))
    (let ((entry (assoc keyword (getf (document-settings document) :todo-keywords)
                        :test #'string=)))
      (unless entry
        (edit-signal-error target :invalid-value
                           "TODO keyword is not configured for this document"))
      entry)))

(defun set-heading-todo (document target keyword)
  "Set a headline TODO keyword, or clear it when KEYWORD is NIL."
  (let* ((headline (edit-headline-resolve-target document target))
         (entry (edit-headline-todo-entry document keyword target)))
    (multiple-value-bind (marker-end todo-start todo-end)
        (edit-headline-prefix-layout document headline)
      (let ((patches
              (cond (todo-start
                     (if keyword
                         (list (edit-make-patch todo-start todo-end keyword target))
                         (list (edit-make-patch todo-start todo-end "" target))))
                    (keyword
                     (list (edit-make-patch marker-end marker-end
                                            (concatenate 'string " " keyword)
                                            target))))))
        (edit-headline-apply document headline target patches
                             (list :todo-keyword keyword
                                   :todo-type (and entry (cdr entry))))))))

(defun edit-priority-object-value (priority)
  (cond
    ((and (integerp priority) (<= 0 priority 64))
     (values priority :numeric (princ-to-string priority) t))
    ((characterp priority)
     (edit-priority-object-value (string priority)))
    ((stringp priority)
     (cond
       ((and (= (length priority) 1)
             (<= (char-code #\A) (char-code (char priority 0))
                 (char-code #\Z)))
        (values (char-code (char priority 0)) :alphabetic priority t))
       ((and (plusp (length priority))
             (every #'digit-char-p priority)
             (<= (parse-integer priority) 64))
        (values (parse-integer priority) :numeric priority t))
       (t (values nil nil nil nil))))
    (t (values nil nil nil nil))))

(defun edit-priority-current-value (priority)
  (if (and (integerp priority)
           (<= (char-code #\A) priority (char-code #\Z)))
      (values priority :alphabetic (string (code-char priority)) t)
      (edit-priority-object-value priority)))

(defun edit-priority-keyword (document)
  (let* ((section (find :section (node-children (document-root document))
                        :key #'node-type))
         (keyword (and section
                       (find-if
                        (lambda (node)
                          (and (eq (node-type node) :keyword)
                               (string-equal (node-property node :key)
                                             "PRIORITIES")))
                        (node-children section))))
         (value (and keyword (node-property keyword :value))))
    (values value (not (null keyword)))))

(defun edit-priority-directive-tokens (value)
  (loop with length = (length value)
        with position = 0
        with tokens = nil
        do (loop while (and (< position length)
                            (find (char value position) '(#\Space #\Tab)))
                 do (incf position))
           (when (= position length)
             (return (nreverse tokens)))
           (let ((start position))
             (loop while (and (< position length)
                              (not (find (char value position)
                                         '(#\Space #\Tab))))
                   do (incf position))
             (push (subseq value start position) tokens))))

(defun edit-priority-setting-value (value)
  (cond
    ((and (integerp value) (<= (char-code #\A) value (char-code #\Z)))
     (values value :alphabetic t))
    ((and (stringp value)
          (plusp (length value))
          (every #'digit-char-p value))
     (let ((number (parse-integer value)))
       (if (<= 65 number (char-code #\Z))
           (values number :alphabetic t)
           (multiple-value-bind (number kind token valid-p)
               (edit-priority-object-value value)
             (declare (ignore token))
             (values number kind valid-p)))))
    (t
     (multiple-value-bind (number kind token valid-p)
         (edit-priority-object-value value)
       (declare (ignore token))
       (values number kind valid-p)))))

(defun edit-priority-valid-in-range-p (value kind highest lowest policy-kind)
  (and (eq kind policy-kind)
       (integerp value)
       (<= highest value lowest)))

(defun edit-priority-settings (document target)
  (multiple-value-bind (directive directive-p)
      (edit-priority-keyword document)
    (if directive-p
        (let ((tokens (and (stringp directive)
                           (edit-priority-directive-tokens directive))))
          (unless (= (length tokens) 3)
            (edit-signal-error target :invalid-value
                               "PRIORITIES must contain highest, lowest, and default"))
          (multiple-value-bind (highest highest-kind highest-token highest-valid-p)
              (edit-priority-object-value (first tokens))
            (declare (ignore highest-token))
            (multiple-value-bind (lowest lowest-kind lowest-token lowest-valid-p)
                (edit-priority-object-value (second tokens))
              (declare (ignore lowest-token))
              (multiple-value-bind (default default-kind default-token default-valid-p)
                  (edit-priority-object-value (third tokens))
                (declare (ignore default-token))
                (unless (and highest-valid-p lowest-valid-p default-valid-p
                             (eq highest-kind lowest-kind)
                             (eq highest-kind default-kind)
                             (<= highest default lowest))
                  (edit-signal-error target :invalid-value
                                     "PRIORITIES values must share an ordered range"))
                (values highest lowest default highest-kind)))))
        (let* ((settings (document-settings document))
               (setting-present-p (member :priority-default settings :test #'eq))
               (setting-value (and setting-present-p
                                   (getf settings :priority-default))))
          (if setting-present-p
              (multiple-value-bind (value kind valid-p)
                  (edit-priority-setting-value setting-value)
                (values (char-code #\A) (char-code #\C)
                        (if (and valid-p
                                 (edit-priority-valid-in-range-p
                                  value kind (char-code #\A) (char-code #\C)
                                  :alphabetic))
                            value nil)
                        :alphabetic))
              (values (char-code #\A) (char-code #\C) (char-code #\B)
                      :alphabetic))))))

(defun edit-headline-priority-value (priority target)
  (multiple-value-bind (value kind token valid-p)
      (edit-priority-object-value priority)
    (unless valid-p
      (edit-signal-error target :invalid-value
                         "Priority must be an uppercase letter or an integer from 0 to 64"))
    (values token value kind)))

(defun set-heading-priority (document target priority)
  "Set a priority within the document's PRIORITIES range, or clear it."
  (let* ((headline (edit-headline-resolve-target document target)))
    (multiple-value-bind (highest lowest default policy-kind)
        (edit-priority-settings document target)
      (declare (ignore default))
      (multiple-value-bind (replacement priority-value priority-kind)
          (if priority
              (edit-headline-priority-value priority target)
              (values nil nil nil))
        (when (and priority
                   (not (edit-priority-valid-in-range-p
                         priority-value priority-kind highest lowest policy-kind)))
          (edit-signal-error target :invalid-value
                             "Priority is outside the document's PRIORITIES range"))
      (multiple-value-bind (marker-end todo-start todo-end priority-start priority-end)
          (edit-headline-prefix-layout document headline)
        (declare (ignore todo-start))
        (let ((patches
                (cond (priority-start
                       (if replacement
                           (list (edit-make-patch
                                  priority-start priority-end
                                  (format nil "[#~A]" replacement) target))
                           (list (edit-make-patch priority-start priority-end
                                                  "" target))))
                      (replacement
                       (let ((position (or todo-end marker-end)))
                         (list (edit-make-patch
                                position position
                                (format nil " [#~A]" replacement) target)))))))
          (edit-headline-apply document headline target patches
                               (list :priority priority-value))))))))

(defun adjust-heading-priority (document target direction)
  "Move a headline priority one step up or down within its PRIORITIES range.
An unset priority starts at the configured default before the adjustment."
  (unless (member direction '(:up :down) :test #'eq)
    (edit-signal-error target :invalid-value
                       "Priority direction must be :UP or :DOWN"))
  (let* ((headline (edit-headline-resolve-target document target)))
    (multiple-value-bind (highest lowest default policy-kind)
        (edit-priority-settings document target)
      (multiple-value-bind (current current-kind token current-valid-p)
          (if (node-property headline :priority)
              (edit-priority-current-value
               (node-property headline :priority))
              (values default policy-kind nil t))
        (declare (ignore token))
        (unless (and current-valid-p
                     (edit-priority-valid-in-range-p
                      current current-kind highest lowest policy-kind))
          (edit-signal-error target :invalid-value
                             "Current priority is outside the document's PRIORITIES range"))
        (let* ((next (max highest
                          (min lowest
                               (+ current (if (eq direction :up) -1 1)))))
               (value (if (eq policy-kind :numeric)
                          next
                          (code-char next))))
          (set-heading-priority document headline value))))))

(defun edit-headline-title-value (document raw-title target)
  (unless (and (stringp raw-title)
               (not (find-if (lambda (character)
                               (find character '(#\Newline #\Return)))
                             raw-title)))
    (edit-signal-error target :invalid-value
                       "Headline title must be a single-line string"))
  (let* ((probe (parse-org (format nil "* ~A~%" raw-title)
                           :settings (document-settings document)
                           :limits (document-limits document)))
         (headline (find :headline (node-children (document-root probe))
                         :key #'node-type))
         (normalized (string-trim '(#\Space #\Tab) raw-title)))
    (unless (and headline
                 (string= normalized (node-property headline :raw-value)))
      (edit-signal-error target :invalid-value
                         "Title text contains headline syntax"))
    (node-property headline :title)))

(defun replace-heading-title (document target raw-title)
  "Replace a headline title with RAW-TITLE, preserving its inline Org markup."
  (let* ((headline (edit-headline-resolve-target document target))
         (title (edit-headline-title-value document raw-title target))
         (normalized (string-trim '(#\Space #\Tab) raw-title)))
      (multiple-value-bind (start end)
        (edit-headline-title-span document headline)
      (multiple-value-bind (tag-start tag-end)
          (edit-headline-tag-span document headline)
        (declare (ignore tag-end))
        (let ((replacement
                (if (and (= start end) tag-start (plusp (length raw-title)))
                    (concatenate 'string raw-title " ")
                    raw-title)))
          (edit-headline-apply
           document headline target
           (list (edit-make-patch start end replacement target))
           (list :raw-value normalized :title title)))))))

(defun edit-headline-validate-tags (tags target)
  (unless (edit-proper-list-length tags)
    (edit-signal-error target :invalid-value
                       "Tags must be a proper list of tag strings"))
  (unless (every (lambda (tag)
                   (and (stringp tag) (reader-heading-valid-tag-name-p tag)))
                 tags)
    (edit-signal-error target :invalid-value
                       "Each tag must be a valid Org tag name"))
  tags)

(defun edit-headline-format-tags (tags)
  (if tags
      (format nil ":~{~A~^:~}:" tags)
      ""))

(defun edit-headline-tag-patch (document headline target tags)
  (multiple-value-bind (tag-start tag-end)
      (edit-headline-tag-span document headline)
    (cond (tag-start
           (list (edit-make-patch tag-start tag-end
                                  (edit-headline-format-tags tags) target)))
          (tags
           (multiple-value-bind (title-start position)
               (edit-headline-title-span document headline)
             (declare (ignore title-start))
             (list (edit-make-patch
                    position position
                    (concatenate 'string " "
                                 (edit-headline-format-tags tags))
                    target))))
          (t nil))))

(defun edit-headline-set-tags (document target tags)
  (let* ((headline (edit-headline-resolve-target document target))
         (tags (edit-headline-validate-tags tags target)))
    (edit-headline-apply
     document headline target
     (edit-headline-tag-patch document headline target tags)
     (list :tags tags
           :archivedp (if (member "ARCHIVE" tags :test #'string=) t nil)))))

(defun add-heading-tags (document target tags)
  "Append new TAGS to a headline while preserving its existing tag order."
  (let* ((headline (edit-headline-resolve-target document target))
         (tags (edit-headline-validate-tags tags target))
         (combined (copy-list (node-property headline :tags))))
    (dolist (tag tags)
      (unless (member tag combined :test #'string=)
        (setf combined (append combined (list tag)))))
    (edit-headline-set-tags document target combined)))

(defun remove-heading-tags (document target tags)
  "Remove the named TAGS from a headline while retaining the other tag order."
  (let* ((headline (edit-headline-resolve-target document target))
         (tags (edit-headline-validate-tags tags target))
         (current (node-property headline :tags))
         (remaining (remove-if (lambda (tag)
                                 (member tag tags :test #'string=))
                               current)))
    (edit-headline-set-tags document target remaining)))

(defun replace-heading-tags (document target tags)
  "Replace a headline's tags with TAGS in the order given."
  (edit-headline-set-tags document target tags))

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

(defun edit-headline-priority-value (priority target)
  (when priority
    (let ((character (cond ((characterp priority) priority)
                           ((and (stringp priority) (= (length priority) 1))
                            (char priority 0)))))
      (unless (and character
                   (or (<= (char-code #\A) (char-code character)
                           (char-code #\Z))
                       (digit-char-p character)))
        (edit-signal-error target :invalid-value
                           "Priority must be one uppercase letter or digit"))
      (values (string character)
              (if (digit-char-p character)
                  (digit-char-p character)
                  (char-code character))))))

(defun set-heading-priority (document target priority)
  "Set a headline priority to one uppercase letter or digit, or clear it."
  (let* ((headline (edit-headline-resolve-target document target)))
    (multiple-value-bind (replacement priority-value)
        (edit-headline-priority-value priority target)
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
                               (list :priority priority-value)))))))

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

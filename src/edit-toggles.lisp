;;;; Format-preserving edits for Org line toggles.
(in-package #:org-kit)

(defun edit-toggle-check-document (document)
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  document)

(defun edit-toggle-validate-span (document span target)
  (edit-toggle-check-document document)
  (let ((length (length (document-text document))))
    (unless (and (edit-span-p span)
                 (integerp (edit-span-start span))
                 (integerp (edit-span-end span))
                 (<= 0 (edit-span-start span)
                     (edit-span-end span) length))
      (edit-signal-error target :invalid-value
                         "Edit span is invalid for this document")))
  span)

(defun edit-toggle-line-start (source offset)
  (let ((newline (position #\Newline source :end offset :from-end t)))
    (if newline (1+ newline) 0)))

(defun edit-toggle-line-end (source start)
  (let ((newline (position #\Newline source :start start)))
    (if (and newline (> newline start)
             (char= (char source (1- newline)) #\Return))
        (1- newline)
        (or newline (length source)))))

(defun edit-toggle-selected-lines (document span target)
  (edit-toggle-validate-span document span target)
  (let* ((source (document-text document))
         (start (edit-span-start span))
         (end (edit-span-end span))
         (first-line (edit-toggle-line-start source start))
         (last-line (if (= start end)
                        first-line
                        (edit-toggle-line-start source (1- end))))
         (lines '())
         (line-start first-line))
    (loop
      for line-end = (edit-toggle-line-end source line-start)
      do (push (list line-start line-end
                     (subseq source line-start line-end))
               lines)
         (when (= line-start last-line) (return))
         (setf line-start (1+ (position #\Newline source :start line-start))))
    (nreverse lines)))

(defun edit-toggle-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun edit-toggle-indentation-end (line)
  (loop for position from 0 below (length line)
        while (edit-toggle-horizontal-space-p (char line position))
        finally (return position)))

(defun edit-toggle-blank-line-p (line)
  (= (edit-toggle-indentation-end line) (length line)))

(defun edit-toggle-heading-prefix-end (line)
  (let ((end (loop for position from 0 below (length line)
                   while (char= (char line position) #\*)
                   finally (return position))))
    (when (and (plusp end) (< end (length line))
               (edit-toggle-horizontal-space-p (char line end)))
      (loop while (and (< end (length line))
                       (edit-toggle-horizontal-space-p (char line end)))
            do (incf end)
            finally (return end)))))

(defun edit-toggle-item-prefix (line)
  (let* ((indent-end (edit-toggle-indentation-end line))
         (length (length line))
         (marker-end
           (cond
             ((and (< indent-end length)
                   (find (char line indent-end) '(#\- #\+ #\*)))
              (1+ indent-end))
             ((and (< indent-end length)
                   (digit-char-p (char line indent-end)))
              (let ((position
                      (loop for index from indent-end below length
                            while (digit-char-p (char line index))
                            finally (return index))))
                (when (and (< position length)
                           (find (char line position) '(#\. #\))))
                  (1+ position)))))))
    (when (and marker-end (< marker-end length)
               (edit-toggle-horizontal-space-p (char line marker-end)))
      (values indent-end
              (loop for position from marker-end below length
                    while (edit-toggle-horizontal-space-p (char line position))
                    finally (return position))))))

(defun edit-toggle-fixed-marker-end (line)
  (let ((indent-end (edit-toggle-indentation-end line)))
    (when (and (< indent-end (length line))
               (char= (char line indent-end) #\:)
               (or (= (1+ indent-end) (length line))
                   (edit-toggle-horizontal-space-p
                    (char line (1+ indent-end)))))
      (if (and (< (1+ indent-end) (length line))
               (edit-toggle-horizontal-space-p
                (char line (1+ indent-end))))
          (+ indent-end 2)
          (1+ indent-end)))))

(defun edit-toggle-line-kind (line)
  (cond
    ((edit-toggle-blank-line-p line) :blank)
    ((edit-toggle-heading-prefix-end line) :headline)
    ((nth-value 1 (edit-toggle-item-prefix line)) :item)
    ((edit-toggle-fixed-marker-end line) :fixed-width)
    ((and (plusp (length line)) (char= (char line 0) #\#)) :directive)
    (t :normal)))

(defun edit-toggle-protected-line-p (document start end allowed-types)
  (let ((protected nil))
    (map-nodes
     (lambda (node)
       (let ((node-start (node-begin node))
             (node-end (node-end node)))
         (when (and (member (node-type node)
                            '(:src-block :example-block :export-block
                              :comment :comment-block :dynamic-block
                              :quote-block :verse-block :table :drawer
                              :property-drawer :footnote-definition :plain-list
                              :item :fixed-width :keyword :clock
                              :horizontal-rule :babel-call :diary-sexp))
                    (not (member (node-type node) allowed-types))
                    node-start node-end
                    (< node-start (max (1+ start) end))
                    (> node-end start))
           (setf protected t))))
     (document-root document))
    protected))

(defun edit-toggle-line-headline-node (document line-start)
  (let ((headline nil))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :headline)
                  (or (= (or (node-post-affiliated node) -1) line-start)
                      (= (or (node-begin node) -1) line-start)))
         (setf headline node)))
     (document-root document))
    headline))

(defun edit-toggle-nearest-heading-level (document source line-start)
  (declare (ignore document))
  (let ((cursor 0)
        (level nil))
    (loop while (< cursor line-start)
          for line-end = (edit-toggle-line-end source cursor)
          for line = (subseq source cursor line-end)
          for heading-end = (edit-toggle-heading-prefix-end line)
          do (when heading-end
               (setf level
                     (loop for position from 0 below heading-end
                           count (char= (char line position) #\*))))
             (setf cursor (1+ (position #\Newline source :start cursor))))
    (1+ (or level 0))))

(defun edit-toggle-result (document patches target)
  (let* ((source (document-text document))
         (prepared (edit-prepare-patches document patches :target target))
         (new-source (edit-splice-source
                      source (edit-prepared-patches-patches prepared)))
         (preview (edit-reparse-source document new-source target)))
    (edit-apply-patches-result
     document patches :target target
     :expected (edit-canonical-document preview))))

(defun edit-toggle-line-patch (line-start line-end replacement target)
  (edit-make-patch line-start line-end replacement target))

(defun edit-toggle-first-content-line (lines)
  (find-if (lambda (line) (not (eq (edit-toggle-line-kind (third line)) :blank)))
           lines))

(defun toggle-heading-comment (document span)
  "Toggle COMMENT on the headline located by SPAN and return an edit result."
  (let* ((target "headline")
         (lines (edit-toggle-selected-lines document span target))
         (line (edit-toggle-first-content-line lines))
         (source (document-text document)))
    (unless line
      (edit-signal-error target :target-not-found
                         "No headline is located by the edit span"))
    (let* ((line-start (first line))
           (line-end (second line))
           (headline (edit-toggle-line-headline-node document line-start)))
      (unless headline
        (edit-signal-error target :wrong-node-type
                           "Edit span does not locate a headline"))
      (multiple-value-bind (marker-end todo-start todo-end priority-start priority-end)
          (edit-headline-prefix-layout document headline)
        (declare (ignore todo-start priority-start))
        (let* ((prefix-end (max marker-end (or todo-end marker-end)
                               (or priority-end marker-end)))
               (position (edit-headline-skip-space source prefix-end line-end))
               (commented-p (node-property headline :commentedp))
               (comment-end (+ position (length "COMMENT")))
               (comment-present-p
                 (and (<= comment-end line-end)
                      (string= "COMMENT" source
                               :start2 position :end2 comment-end)
                      (or (= comment-end line-end)
                          (edit-toggle-horizontal-space-p
                           (char source comment-end))))))
          (unless (eq (not (null commented-p)) (not (null comment-present-p)))
            (edit-signal-error target :uneditable-structure
                               "Headline COMMENT marker does not match its parsed state"))
          (let* ((patch
                   (if commented-p
                       (let ((end comment-end))
                         (loop while (and (< end line-end)
                                          (edit-toggle-horizontal-space-p
                                           (char source end)))
                               do (incf end))
                         (edit-make-patch position end "" target))
                       (edit-make-patch
                        position position
                        (concatenate
                         'string
                         (if (and (> position line-start)
                                  (not (edit-toggle-horizontal-space-p
                                        (char source (1- position)))))
                             " " "")
                         "COMMENT"
                         (if (< position line-end) " " ""))
                        target)))
                 (expected (edit-canonical-copy
                            (edit-canonical-document document)))
                 (path (edit-node-path document headline target)))
            (edit-canonical-set-property expected path :commentedp
                                         (not commented-p))
            (edit-apply-patches-result document (list patch)
                                       :target headline :expected expected)))))))

(defun toggle-heading (document span)
  "Toggle the selected lines between headlines, list items, and ordinary text."
  (let* ((target "heading")
         (lines (edit-toggle-selected-lines document span target))
         (first-line (edit-toggle-first-content-line lines))
         (first-kind (and first-line
                          (edit-toggle-line-kind (third first-line))))
         (source (document-text document))
         (patches '()))
    (unless first-line
      (return-from toggle-heading
                   (edit-toggle-result document nil target)))
    (dolist (line lines)
      (destructuring-bind (line-start line-end text) line
        (let ((kind (edit-toggle-line-kind text)))
          (unless (or (eq kind :blank)
                      (eq kind :directive)
                      (edit-toggle-protected-line-p
                       document line-start line-end
                       '(:org-data :section :paragraph :headline :plain-list
                                   :item)))
            (cond
             ((eq first-kind :headline)
              (when (eq kind :headline)
                (let ((prefix-end (edit-toggle-heading-prefix-end text)))
                  (push (edit-toggle-line-patch
                         line-start (+ line-start prefix-end) "" target)
                        patches))))
             ((eq first-kind :item)
              (when (eq kind :item)
                (let* ((content-start
                        (nth-value 1 (edit-toggle-item-prefix text)))
                       (checkbox-p
                        (and (<= (+ content-start 3) (length text))
                             (char= (char text content-start) #\[)
                             (char= (char text (+ content-start 2)) #\])
                             (find (char text (1+ content-start))
                                   (list #\Space #\X #\x #\-))))
                       (done-p (and checkbox-p
                                    (find (char text (1+ content-start)) (list #\X #\x))))
                       (content-offset
                        (if checkbox-p
                            (loop for position from (+ content-start 3)
                                  below (length text)
                                  while (edit-toggle-horizontal-space-p (char text position))
                                  finally (return position))
                          content-start))
                       (level (edit-toggle-nearest-heading-level document source line-start))
                       (replacement
                        (concatenate (quote string)
                                     (make-string level :initial-element #\*)
                                     " "
                                     (if checkbox-p
                                         (if done-p "DONE " "TODO ")
                                       "")
                                     (subseq text content-offset))))
                  (push (edit-toggle-line-patch
                         line-start line-end replacement target)
                        patches))))
             (t
              (when (eq kind :normal)
                (let* ((indent-end (edit-toggle-indentation-end text))
                       (level (edit-toggle-nearest-heading-level
                               document source line-start))
                       (replacement
                        (concatenate 'string (make-string level :initial-element #\*) " "
                                     (subseq text indent-end))))
                  (push (edit-toggle-line-patch
                         line-start line-end replacement target)
                        patches)))))))))
    (edit-toggle-result document (nreverse patches) target)))

(defun toggle-item (document span)
  "Toggle the selected lines between Org list items, headlines, and ordinary text."
  (let* ((target "item")
         (lines (edit-toggle-selected-lines document span target))
         (first-line (edit-toggle-first-content-line lines))
         (first-kind (and first-line
                          (edit-toggle-line-kind (third first-line))))
         (patches '())
         (first-heading-level
           (and first-line (eq first-kind :headline)
                (loop for position below
                      (edit-toggle-heading-prefix-end (third first-line))
                      count (char= (char (third first-line) position) #\*))))
         (source (document-text document)))
    (unless first-line
      (return-from toggle-item
        (edit-toggle-result document nil target)))
    (dolist (line lines)
      (destructuring-bind (line-start line-end text) line
        (let ((kind (edit-toggle-line-kind text)))
          (unless (or (eq kind :blank)
                      (eq kind :directive)
                      (edit-toggle-protected-line-p
                       document line-start line-end
                       '(:org-data :section :paragraph :headline :plain-list
                         :item)))
            (cond
              ((eq first-kind :item)
               (when (eq kind :item)
                 (multiple-value-bind (indent-end content-start)
                     (edit-toggle-item-prefix text)
                   (push (edit-toggle-line-patch
                          (+ line-start indent-end)
                          (+ line-start content-start)
                          "" target)
                         patches))))
              ((eq first-kind :headline)
               (when (eq kind :headline)
                 (let* ((headline (edit-toggle-line-headline-node
                                   document line-start))
                        (title-start nil)
                        (title-end nil))
                   (unless headline
                     (edit-signal-error target :uneditable-structure
                                        "Headline source cannot be resolved"))
                   (multiple-value-setq (title-start title-end)
                     (edit-headline-title-span document headline))
                   (let* ((level
                            (loop for position below
                                  (edit-toggle-heading-prefix-end text)
                                  count (char= (char text position) #\*)))
                          (indent (* 2 (max 0 (- level
                                                (or first-heading-level level)))))
                          (keyword (node-property headline :todo-keyword))
                          (checkbox (when keyword
                                      (if (eq (node-property headline :todo-type)
                                              :done)
                                          "[X] " "[ ] ")))
                          (replacement
                            (concatenate
                             'string (make-string indent :initial-element #\Space) "- "
                             (or checkbox "")
                             (subseq source title-start title-end))))
                     (push (edit-toggle-line-patch
                            line-start line-end replacement target)
                           patches)))))
              (t
               (when (eq kind :normal)
                 (let ((indent-end (edit-toggle-indentation-end text)))
                   (push (edit-toggle-line-patch
                          (+ line-start indent-end)
                          (+ line-start indent-end)
                          "- " target)
                         patches)))))))))
    (edit-toggle-result document (nreverse patches) target)))

(defun toggle-fixed-width (document span)
  "Toggle fixed-width markers on the selected lines and return an edit result."
  (let* ((target "fixed-width")
         (lines (edit-toggle-selected-lines document span target))
         (has-content-p nil)
         (all-fixed-p t)
         (patches '()))
    (dolist (line lines)
      (let ((text (third line)))
        (unless (edit-toggle-blank-line-p text)
          (setf has-content-p t)
          (unless (edit-toggle-fixed-marker-end text)
            (setf all-fixed-p nil)))))
    (unless has-content-p (setf all-fixed-p nil))
    (dolist (line lines)
      (destructuring-bind (line-start line-end text) line
        (let* ((fixed-end (edit-toggle-fixed-marker-end text))
               (indent-end (edit-toggle-indentation-end text)))
          (when (and (not fixed-end)
                     (edit-toggle-protected-line-p
                      document line-start line-end
                      '(:org-data :section :paragraph :headline :keyword
                        :comment :planning :clock :horizontal-rule :babel-call
                        :diary-sexp :fixed-width)))
            (edit-signal-error target :uneditable-structure
                               "Fixed-width markup cannot be inserted here"))
          (if all-fixed-p
              (when fixed-end
                (push (edit-toggle-line-patch
                       (+ line-start indent-end) (+ line-start fixed-end) ""
                       target)
                      patches))
              (unless fixed-end
                (push (edit-toggle-line-patch
                       (+ line-start indent-end) (+ line-start indent-end)
                       ": " target)
                      patches))))))
    (edit-toggle-result document (nreverse patches) target)))

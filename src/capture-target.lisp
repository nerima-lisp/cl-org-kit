;;;; src/capture-target.lisp
;;;; Resolve capture targets and return a format-preserving edit result.
(in-package #:org-kit)

(defstruct
    (capture-target-result
     (:constructor make-capture-target-result
                   (&key edit-result target-document cursor-offset))
     (:conc-name %capture-target-result-)
     (:copier nil)
     (:predicate capture-target-result-p))
  "The edit result and post-insertion cursor for one capture target."
  edit-result
  target-document
  cursor-offset)

(defun capture-target-result-edit-result (result)
  "Return RESULT's edit result."
  (%capture-target-result-edit-result result))

(defun capture-target-result-target-document (result)
  "Return the updated target document."
  (%capture-target-result-target-document result))

(defun capture-target-result-cursor-offset (result)
  "Return the cursor's absolute offset in the updated target document."
  (%capture-target-result-cursor-offset result))

(defun capture-target-signal (reason context message)
  (capture-signal reason context message))

(defun capture-target-normalize-documents (documents)
  (let ((normalized
         (cond
           ((document-p documents) (list documents))
           ((and (vectorp documents) (not (stringp documents)))
            (coerce documents 'list))
           (t documents))))
    (unless (and (listp normalized) (edit-proper-list-length normalized))
      (capture-target-signal :invalid-documents
                             nil
                             "Capture documents must be a proper document sequence"))
    (unless (every #'document-p normalized)
      (capture-target-signal :invalid-documents
                             nil
                             "Every capture document must be an Org document"))
    normalized))

(defun capture-target-headlines (document)
  (let ((headlines nil))
    (when (document-root document)
      (map-nodes
       (lambda (node)
         (when (eq (node-type node) :headline)
           (push node headlines)))
       (document-root document)))
    (nreverse headlines)))

(defun capture-target-title (headline)
  (string-right-trim '(#\Return) (node-property headline :raw-value)))

(defun capture-target-direct-headlines (document parent)
  (remove-if-not
   (lambda (node)
     (eq (node-type node) :headline))
   (node-children (or parent (document-root document)))))

(defun capture-target-find-direct-child (document parent title)
  (find title
        (capture-target-direct-headlines document parent)
        :key
        #'capture-target-title
        :test
        #'string=))

(defun capture-target-star-count (document headline)
  (let* ((source (document-text document))
         (start (node-post-affiliated headline))
         (end
          (loop for index from start below (length source)
                while (char= (char source index) #\*)
                finally (return index))))
    (- end start)))

(defun capture-target-parent-insertion-offset (document parent)
  (if parent
      (nth-value 1 (edit-subtree-next-boundary document parent))
      (length (document-text document))))

(defun capture-target-file-document (documents file file-contents limits)
  (let ((matches
         (remove-if-not
          (lambda (document)
            (equal (document-source-name document) file))
          documents)))
    (cond
      ((> (length matches) 1)
       (capture-target-signal :ambiguous-target
                              file
                              "More than one document has the target file name"))
      (matches (first matches))
      (t
       (multiple-value-bind (contents found-p)
           (capture-template-assoc-value file file-contents)
         (unless found-p
           (capture-target-signal :target-not-found
                                  file
                                  "No parsed document or supplied contents match the target file"))
         (unless (stringp contents)
           (capture-target-signal :invalid-target
                                  file
                                  "Supplied target file contents must be a string"))
         (parse-org contents :source-name file :limits limits))))))

(defun capture-target-template-headlines (fragment)
  (let ((nodes nil))
    (when (document-root fragment)
      (map-nodes
       (lambda (node)
         (when (eq (node-type node) :headline)
           (push node nodes)))
       (document-root fragment)))
    (stable-sort (nreverse nodes) #'< :key #'node-post-affiliated)))

(defun capture-target-transform-template (document text
                                                   cursor-offset
                                                   parent-stars)
  (let* ((limits (document-limits document))
         (fragment
          (parse-org text :limits limits :settings (document-settings document)))
         (headlines (capture-target-template-headlines fragment))
         (root-headlines
          (remove-if-not
           (lambda (node)
             (eq (node-type node) :headline))
           (node-children (document-root fragment)))))
    (if (null headlines)
        (let* ((prefix
                (format nil
                        "~A "
                        (make-string (1+ parent-stars) :initial-element #\*))))
          (values (concatenate 'string prefix text)
                  (+ (length prefix) cursor-offset)))
        (let* ((first-stars
                (capture-target-star-count fragment (first root-headlines)))
               (shift (- (1+ parent-stars) first-stars))
               (source (document-text fragment))
               (cursor cursor-offset)
               (position 0)
               (inserted-before-cursor 0)
               (transformed
                (with-output-to-string (stream)
                  (dolist (headline headlines)
                    (let* ((start (node-post-affiliated headline))
                           (old-stars
                            (capture-target-star-count fragment headline))
                           (new-stars (+ old-stars shift)))
                      (when (<= (+ start old-stars) cursor)
                        (incf inserted-before-cursor shift))
                      (when (<= new-stars 0)
                        (capture-target-signal :invalid-template
                                               nil
                                               "Capture outline levels cannot be shifted to the target"))
                      (write-string (subseq source position start) stream)
                      (write-string (make-string new-stars :initial-element #\*)
                                    stream)
                      (setf position (+ start old-stars))))
                  (write-string (subseq source position) stream))))
          (values transformed (+ cursor inserted-before-cursor))))))

(defun capture-target-ensure-path (document path)
  (let ((parent nil)
        (remaining (copy-list path)))
    (loop while remaining
          for title = (pop remaining)
          for child = (capture-target-find-direct-child document parent title)
          do (if child
                 (setf parent child)
                 (progn
                   (push title remaining)
                   (return))))
    (if (null remaining)
        (values parent
                (capture-target-parent-insertion-offset document parent)
                ""
                (if parent
                    (capture-target-star-count document parent)
                    0))
        (let* ((offset (capture-target-parent-insertion-offset document parent))
               (stars
                (if parent
                    (capture-target-star-count document parent)
                    0))
               (newline (edit-subtree-line-ending document offset))
               (prefix
                (with-output-to-string (stream)
                  (dolist (title remaining)
                    (incf stars)
                    (format stream
                            "~A ~A~A"
                            (make-string stars :initial-element #\*)
                            title
                            newline)))))
          (values nil offset prefix stars)))))

(defun capture-target-month-name (month)
  (nth (1- month)
       '("January" "February"
                   "March"
                   "April"
                   "May"
                   "June"
                   "July"
                   "August"
                   "September"
                   "October"
                   "November"
                   "December")))

(defun capture-target-day-name (value)
  (edit-todo-weekday-name value))

(defun capture-target-datetree-path (time-context)
  (let* ((value (org-time-context-reference-time time-context))
         (year (org-time-value-year value))
         (month (org-time-value-month value))
         (day (org-time-value-day value)))
    (list (format nil "~4,'0D" year)
          (format nil
                  "~4,'0D-~2,'0D ~A"
                  year
                  month
                  (capture-target-month-name month))
          (format nil
                  "~4,'0D-~2,'0D-~2,'0D ~A"
                  year
                  month
                  day
                  (capture-target-day-name value)))))

(defun capture-target-regexp-location (document regexp limits)
  (let* ((text (document-text document))
         (pattern-limit (limit-value limits :max-search-regex-length))
         (depth-limit (limit-value limits :max-element-depth))
         (regex
          (handler-case (progn
                          (check-limit limits
                                       :max-search-regex-length
                                       (length regexp))
                          (check-limit limits :max-search-work (length text))
                          (cl-regex-kit:compile-regex regexp
                                                      :multi-line
                                                      t
                                                      :size-limit
                                                      (max 1 pattern-limit)
                                                      :nest-limit
                                                      depth-limit))
            (cl-regex-kit:regex-syntax-error ()
              (capture-target-signal :invalid-target
                                     regexp
                                     "Target regexp is invalid"))))
         (match (cl-regex-kit:scan regex text)))
    (unless match
      (capture-target-signal :target-not-found
                             regexp
                             "No target regexp match was found"))
    (values (cl-regex-kit:match-start match) (cl-regex-kit:match-end match))))

(defun capture-target-regexp-heading (document start end)
  (let* ((text (document-text document))
         (line-start
          (let ((newline (position #\Newline text :end start :from-end t)))
            (if newline
                (1+ newline)
                0)))
         (line-end (or (position #\Newline text :start end) (length text))))
    (find-if
     (lambda (headline)
       (and (= (node-post-affiliated headline) line-start)
            (<= start line-end)
            (<= line-start end)))
     (capture-target-headlines document))))

(defun capture-target-resolve (documents target-spec
                                         file-contents
                                         limits
                                         time-context)
  (unless (and (listp target-spec) (edit-proper-list-length target-spec))
    (capture-target-signal :invalid-target
                           target-spec
                           "Capture target must be a proper list"))
  (let ((mode (first target-spec)))
    (cond
      ((member mode
               '(:file+headline :file+olp :file+olp+datetree :file+regexp)
               :test
               #'eq)
        (let ((expected-length
               (if (eq mode :file+olp+datetree)
                   2
                   3)))
          (unless (= (length target-spec) expected-length)
            (capture-target-signal :invalid-target
                                   target-spec
                                   "Capture target has the wrong number of elements")))
        (unless (stringp (second target-spec))
          (capture-target-signal :invalid-target
                                 target-spec
                                 "Capture target file must be a string"))
        (let ((document
               (capture-target-file-document documents
                                             (second target-spec)
                                             file-contents
                                             limits)))
          (cond
            ((eq mode :file+headline)
             (let ((title (third target-spec)))
               (unless (stringp title)
                 (capture-target-signal :invalid-target
                                        target-spec
                                        "Headline target must be a string"))
               (let ((headline
                      (find title
                            (capture-target-headlines document)
                            :key
                            #'capture-target-title
                            :test
                            #'string=)))
                 (if headline
                     (values document
                             headline
                             (capture-target-parent-insertion-offset document
                                                                     headline)
                             ""
                             (capture-target-star-count document headline))
                     (let* ((offset (length (document-text document)))
                            (newline (edit-subtree-line-ending document offset)))
                       (values document
                               nil
                               offset
                               (format nil "* ~A~A" title newline)
                               1))))))
            ((eq mode :file+olp)
             (let ((path (third target-spec)))
               (unless
                   (and (listp path)
                        (plusp (length path))
                        (every #'stringp path))
                 (capture-target-signal :invalid-target
                                        target-spec
                                        "Outline path must contain at least one string"))
               (multiple-value-bind (parent offset prefix stars)
                   (capture-target-ensure-path document path)
                 (values document parent offset prefix stars))))
            ((eq mode :file+olp+datetree)
              (unless (org-time-context-p time-context)
                (capture-target-signal :invalid-time-context
                                       :time-context
                                       "Datetree target requires an explicit time context"))
              (multiple-value-bind (parent offset prefix stars)
                  (capture-target-ensure-path document
                                              (capture-target-datetree-path
                                               time-context))
                (values document parent offset prefix stars)))
            (t
             (let ((regexp (third target-spec)))
               (unless (stringp regexp)
                 (capture-target-signal :invalid-target
                                        target-spec
                                        "Target regexp must be a string"))
               (multiple-value-bind (start end)
                   (capture-target-regexp-location document regexp limits)
                 (let ((headline
                        (capture-target-regexp-heading document start end)))
                   (if headline
                       (values document
                               headline
                               (capture-target-parent-insertion-offset document
                                                                       headline)
                               ""
                               (capture-target-star-count document headline))
                       (values document
                               nil
                               (length (document-text document))
                               ""
                               0)))))))))
      ((eq mode :id)
        (unless (and (= (length target-spec) 2) (stringp (second target-spec)))
          (capture-target-signal :invalid-target
                                 target-spec
                                 "ID target must contain one string"))
        (let* ((identifier (second target-spec))
               (index (make-org-id-index documents))
               (candidates
                (handler-case (org-id-index-candidates index identifier)
                  (org-id-error ()
                    (capture-target-signal :invalid-target
                                           identifier
                                           "ID target is invalid")))))
          (cond
            ((null candidates)
             (capture-target-signal :target-not-found
                                    identifier
                                    "No target with the requested ID was found"))
            ((cdr candidates)
             (capture-target-signal :ambiguous-target
                                    identifier
                                    "More than one target has the requested ID"))
            ((not (eq (node-type (first candidates)) :headline))
             (capture-target-signal :invalid-target
                                    identifier
                                    "Capture ID targets must resolve to a headline"))
            (t
             (let* ((parent (first candidates))
                    (document (node-document parent)))
               (values document
                       parent
                       (capture-target-parent-insertion-offset document parent)
                       ""
                       (capture-target-star-count document parent)))))))
      (t
       (capture-target-signal :invalid-target
                              target-spec
                              "Unsupported capture target mode")))))

(defun capture-target-join-fragment (document offset fragment)
  (let* ((source (document-text document))
         (newline (edit-subtree-line-ending document offset))
         (prefix-p
          (and (plusp offset) (not (char= (char source (1- offset)) #\Newline))))
         (prefix
          (if prefix-p
              (edit-subtree-fragment-separator newline
                                               (char= (char source (1- offset))
                                                      #\Return))
              ""))
         (suffix
          (if (and (plusp (length fragment))
                   (not
                    (char= (char fragment (1- (length fragment))) #\Newline)))
              newline
              "")))
    (values (concatenate 'string prefix fragment suffix) (length prefix))))

(defun capture-target-apply-insertion (document offset
                                                prefix
                                                template
                                                cursor-offset
                                                parent-stars)
  (multiple-value-bind (adjusted-template adjusted-cursor)
      (capture-target-transform-template document
                                         template
                                         cursor-offset
                                         parent-stars)
    (let* ((fragment (concatenate 'string prefix adjusted-template))
           (source (document-text document)))
      (multiple-value-bind (replacement leading-length)
          (capture-target-join-fragment document offset fragment)
        (let* ((new-source
                (concatenate 'string
                             (subseq source 0 offset)
                             replacement
                             (subseq source offset)))
               (expected-document
                (parse-org new-source
                           :source-name
                           (document-source-name document)
                           :limits
                           (document-limits document)
                           :settings
                           (document-settings document)))
               (edit-result
                (edit-apply-patches-result document
                                           (list
                                            (edit-make-patch offset
                                                             offset
                                                             replacement))
                                           :target
                                           "capture target"
                                           :expected
                                           (edit-canonical-document
                                            expected-document)))
               (cursor
                (+ offset leading-length (length prefix) adjusted-cursor)))
          (make-capture-target-result :edit-result
                                      edit-result
                                      :target-document
                                      (edit-result-updated-document edit-result)
                                      :cursor-offset
                                      cursor))))))

(defun capture-into-target (documents target-spec
                                      template-result
                                      &key
                                      file-contents
                                      time-context
                                      limits)
  "Insert TEMPLATE-RESULT at TARGET-SPEC and return an edit result.
DOCUMENTS are parsed Org documents; missing file documents can be supplied as
FILE-CONTENTS alist entries. This function never reads or writes a file."
  (unless (capture-template-result-p template-result)
    (capture-target-signal :invalid-template-result
                           nil
                           "Expected an expanded capture template result"))
  (unless (org-time-context-p time-context)
    (capture-target-signal :invalid-time-context
                           :time-context
                           "Capture target resolution requires an explicit time context"))
  (let* ((normalized-documents (capture-target-normalize-documents documents))
         (effective-limits
          (or limits
              (and normalized-documents
                   (document-limits (first normalized-documents)))
              (make-limits))))
    (unless (limits-p effective-limits)
      (capture-target-signal :invalid-limits nil "Expected a limits object"))
    (multiple-value-bind (document parent offset prefix parent-stars)
        (capture-target-resolve normalized-documents
                                target-spec
                                file-contents
                                effective-limits
                                time-context)
      (declare (ignore parent))
      (let ((expanded-text
             (capture-template-result-expanded-text template-result))
            (cursor-offset
             (capture-template-result-cursor-offset template-result)))
        (unless
            (and (stringp expanded-text)
                 (integerp cursor-offset)
                 (<= 0 cursor-offset (length expanded-text)))
          (capture-target-signal :invalid-template-result
                                 nil
                                 "Capture template cursor must be within expanded text"))
        (check-limit effective-limits :max-input-length (length expanded-text))
        (capture-target-apply-insertion document
                                        offset
                                        prefix
                                        expanded-text
                                        cursor-offset
                                        parent-stars)))))

;;;; Archive location resolution and archive-sibling edits.
(in-package #:org-kit)

(defun org-archive-signal-error (reason message &optional context)
  (error 'org-archive-error :reason reason :message message
         :context (and context (bounded-text context))))

(defun archive-document-source-path (document)
  (let ((source-name (document-source-name document)))
    (typecase source-name
      (string source-name)
      (pathname (sb-ext:native-namestring source-name))
      (null nil)
      (t (org-archive-signal-error
          :invalid-source-name "Document source name must be a path")))))

(defun archive-source-basename (document limits)
  (labels ((path-component-length (component)
             (typecase component
               (null 0)
               (string (length component))
               (symbol
                (case component
                  (:wild 1)
                  ((:up :back :wild-inferiors) 2)
                  ((:unspecific :newest) 0)
                  (t
                   (org-archive-signal-error
                    :invalid-source-name
                    "Unsupported source path component"))))
               (t
                (org-archive-signal-error
                 :invalid-source-name
                 "Unsupported source path component"))))
           (pathname-file-length (path)
             (+ (path-component-length (pathname-name path))
                (let ((type (pathname-type path)))
                  (if (and type (not (eq type :unspecific)))
                      (1+ (path-component-length type))
                      0)))))
    (let ((source-name (document-source-name document)))
      (typecase source-name
        (string
         (check-limit limits :max-input-length (length source-name))
         (let* ((start (1+ (or (position (char "/" 0) source-name :from-end t) -1)))
                (name-length (- (length source-name) start)))
           (check-limit limits :max-output-length name-length)
           (and (plusp name-length) (subseq source-name start))))
        (pathname
         (when (typep source-name (quote logical-pathname))
           (org-archive-signal-error
            :invalid-source-name
            "Logical source paths cannot be bounded as archive destinations"))
         (let ((name-length (pathname-file-length source-name)))
           (check-limit limits :max-output-length name-length)
           (let ((basename
                   (sb-ext:native-namestring
                    (make-pathname :directory nil :defaults source-name))))
             (check-limit limits :max-output-length (length basename))
             (and (plusp (length basename)) basename))))
        (null nil)
        (t
         (org-archive-signal-error
          :invalid-source-name "Document source name must be a path"))))))

(defun archive-expand-format (document value limits initial-length)
  (let ((basename nil)
        (basename-computed-p nil))
    (with-output-to-string (stream)
      (let ((output-length initial-length))
        (labels ((source-basename ()
                   (unless basename-computed-p
                     (setf basename (archive-source-basename document limits)
                           basename-computed-p t))
                   basename)
                 (write-character (character)
                   (incf output-length)
                   (check-limit limits :max-output-length output-length)
                   (write-char character stream))
                 (write-text (text)
                   (let ((next-length (+ output-length (length text))))
                     (check-limit limits :max-output-length next-length)
                     (setf output-length next-length)
                     (write-string text stream))))
          (loop for index from 0 below (length value)
                for character = (char value index)
                do (if (char= character #\%)
                       (progn
                         (when (= index (1- (length value)))
                           (org-archive-signal-error
                            :invalid-location-format
                            "Archive location ends with a format marker" value))
                         (incf index)
                         (case (char value index)
                           (#\% (write-character #\%))
                           (#\s (let ((basename (source-basename)))
                                  (unless basename
                                    (org-archive-signal-error
                                     :source-name-required
                                     "Archive location uses %s without a source name"
                                     value))
                                  (write-text basename)))
                           (t (org-archive-signal-error
                               :invalid-location-format
                               "Unsupported archive location format" value))))
                       (write-character character))))))))

(defun archive-expand-relative-file (document file limits)
  (labels ((path-component-length (component)
             (typecase component
               (null 0)
               (string (length component))
               (symbol
                (case component
                  (:wild 1)
                  ((:up :back :wild-inferiors) 2)
                  ((:unspecific :newest) 0)
                  (t
                   (org-archive-signal-error
                    :invalid-source-name
                    "Unsupported source path component"))))
               (t
                (org-archive-signal-error
                 :invalid-source-name
                 "Unsupported source path component"))))
           (directory-length (directory)
             (+ (if (eq (first directory) :absolute) 1 0)
                (loop for component in (rest directory)
                      sum (1+ (path-component-length component)))))
           (pathname-length (path)
             (when (typep path 'logical-pathname)
               (org-archive-signal-error
                :invalid-source-name
                "Logical source paths cannot be bounded as archive destinations"))
             (+ (directory-length (pathname-directory path))
                (path-component-length (pathname-name path))
                (let ((type (pathname-type path)))
                  (if (and type (not (eq type :unspecific)))
                      (1+ (path-component-length type))
                      0))))
           (source-directory (source-name file-length)
             (typecase source-name
               (string
                (check-limit limits :max-input-length (length source-name))
                (let* ((source-path (pathname source-name))
                       (directory (pathname-directory source-path)))
                  (when (typep source-path 'logical-pathname)
                    (org-archive-signal-error
                     :invalid-source-name
                     "Logical source paths cannot be bounded as archive destinations"))
                  (check-limit limits :max-output-length
                               (+ (directory-length directory) file-length))
                  (make-pathname :directory directory :name nil :type nil
                                 :defaults source-path)))
               (pathname
                (let ((directory (pathname-directory source-name)))
                  (check-limit limits :max-input-length
                               (pathname-length source-name))
                  (check-limit limits :max-output-length
                               (+ (directory-length directory) file-length))
                  (make-pathname :directory directory :name nil :type nil
                                 :defaults source-name)))
               (t
                (org-archive-signal-error
                 :invalid-source-name "Document source name must be a path")))))
    (let* ((source-name (document-source-name document))
           (file-path (and (plusp (length file)) (pathname file)))
           (file-directory (and file-path (pathname-directory file-path))))
      (cond
        ((zerop (length file))
         (typecase source-name
           (string
            (check-limit limits :max-input-length (length source-name))
            (check-limit limits :max-output-length (length source-name))
            source-name)
           (pathname
            (check-limit limits :max-input-length
                         (pathname-length source-name))
            (check-limit limits :max-output-length
                         (pathname-length source-name))
            (let ((source-path (archive-document-source-path document)))
              (check-limit limits :max-output-length (length source-path))
              source-path))
           (null nil)
           (t
            (org-archive-signal-error
             :invalid-source-name "Document source name must be a path"))))
        ((and file-directory
              (eq (first file-directory) :absolute))
         (check-limit limits :max-output-length (length file))
         file)
        (source-name
         (sb-ext:native-namestring
          (merge-pathnames
           file-path
           (source-directory source-name (length file)))))
        (t
         (check-limit limits :max-output-length (length file))
         file)))))

(defun archive-location-value (document heading default-location)
  (or (loop for candidate in (cons heading
                                   (effective-heading-ancestors heading))
            for properties = (effective-heading-property-entries
                              candidate "ARCHIVE")
            when properties
              return (node-property (first properties) :value))
      (let ((keyword
              (find-if (lambda (node)
                         (and (stringp (node-property node :key))
                              (string-equal (node-property node :key)
                                            "ARCHIVE")))
                       (edit-properties-document-keywords document))))
        (and keyword (node-property keyword :value)))
      default-location
      "%s_archive::"))

(defun resolve-archive-location (document heading &key default-location)
  "Resolve HEADING's archive location as a file and heading string pair."
  (unless (document-p document)
    (org-archive-signal-error :invalid-document
                              "Expected an Org document"))
  (let* ((headline (edit-resolve-node document heading '(:headline)))
         (limits (document-limits document))
         (location (archive-location-value document headline default-location)))
    (unless (stringp location)
      (org-archive-signal-error :invalid-location
                                "Archive location must be a string"))
    (check-limit limits :max-output-length (length location))
    (let ((separator (search "::" location)))
      (unless separator
        (org-archive-signal-error :invalid-location
                                  "Archive location must contain ::" location))
      (let* ((file-format (string-trim '(#\Space #\Tab)
                                       (subseq location 0 separator)))
             (heading-format (subseq location (+ separator 2)))
             (file-fragment
               (archive-expand-format document file-format limits 0))
             (file (archive-expand-relative-file document file-fragment limits))
             (file-length (if file (length file) 0))
             (archive-heading
               (archive-expand-format document heading-format limits
                                      file-length)))
        (check-limit limits :max-output-length
                     (+ file-length (length archive-heading)))
        (cons file archive-heading)))))

(defun archive-time-string (time-context)
  (let* ((zone (org-time-context-zone time-context))
         (instant (%time-value-to-instant
                   (org-time-context-reference-time time-context)
                   zone (org-time-context-dst-policy time-context)))
         (local (cl-date-kit:local-date-time-of-instant
                 instant (org-time-zone-backend zone))))
    (edit-todo-format-point-timestamp local)))

(defun archive-node-at-path (document path)
  (loop with current = (document-root document)
        for index in path
        do (setf current (nth index (node-children current)))
        finally (return current)))

(defun archive-sibling-headings (parent)
  (remove-if-not (lambda (node) (eq (node-type node) :headline))
                 (node-children parent)))

(defun archive-sibling-heading-p (node)
  (and (stringp (node-property node :raw-value))
       (string= (node-property node :raw-value) "Archive")
       (member "ARCHIVE" (node-property node :tags) :test #'string=)))

(defun archive-root-insert-sibling (document heading star-count)
  (let* ((source (document-text document))
         (offset (length source))
         (newline (edit-subtree-line-ending document offset))
         (prefix (if (and (plusp offset)
                          (not (document-final-newline-p document)))
                     newline
                     ""))
         (raw (format nil "~A Archive :ARCHIVE:~A"
                      (make-string star-count :initial-element #\*) newline))
         (replacement (concatenate 'string prefix raw))
         (new-source
           (concatenate 'string source replacement))
         (expected
           (edit-canonical-document
            (edit-reparse-source document new-source heading))))
    (edit-apply-patches-result
     document (list (edit-make-patch offset offset replacement heading))
     :target heading :expected expected)))

(defun archive-final-edit-result (document updated-document target)
  (let* ((before (document-text document))
         (after (document-text updated-document))
         (prefix
           (loop for left from 0 below (min (length before) (length after))
                 while (char= (char before left) (char after left))
                 finally (return left)))
         (suffix
           (loop for offset from 0 below (min (- (length before) prefix)
                                               (- (length after) prefix))
                 while (char= (char before (- (length before) offset 1))
                              (char after (- (length after) offset 1)))
                 finally (return offset)))
         (expected (edit-canonical-document updated-document)))
    (edit-apply-patches-result
     document
     (list (edit-make-patch prefix (- (length before) suffix)
                            (subseq after prefix (- (length after) suffix))
                            target))
     :target target :expected expected)))

(defun archive-to-sibling (document heading &key time-context)
  "Archive HEADING below its tagged Archive sibling and return an edit result."
  (unless (org-time-context-p time-context)
    (if time-context
        (error 'type-error :datum time-context
               :expected-type 'org-time-context)
        (org-archive-signal-error
         :time-context-required
         "Archive edits require an explicit time context")))
  (unless (document-p document)
    (org-archive-signal-error :invalid-document
                              "Expected an Org document"))
  (let* ((source-heading (edit-resolve-node document heading '(:headline)))
         (source-path (edit-node-path document source-heading heading))
         (parent (node-parent source-heading))
         (star-count (nth-value 2 (edit-subtree-marker document source-heading)))
         (time (archive-time-string time-context))
         (timed-document (set-property document source-heading "ARCHIVE_TIME" time))
         (timed-heading (archive-node-at-path timed-document source-path))
         (parent-path (and (eq (node-type parent) :headline)
                           (edit-node-path document parent heading)))
         (timed-parent (and parent-path
                            (archive-node-at-path timed-document parent-path)))
         (archive-sibling
           (find-if #'archive-sibling-heading-p
                    (archive-sibling-headings (or timed-parent
                                                  (document-root timed-document)))))
         (with-sibling-document timed-document))
    (unless archive-sibling
      (setf with-sibling-document
            (if timed-parent
                (insert-subtree
                 timed-document timed-parent
                 (format nil "~A Archive :ARCHIVE:~A"
                         (make-string star-count :initial-element #\*)
                         (edit-subtree-line-ending timed-document
                                                   (length (document-text
                                                            timed-document)))))
                (edit-result-updated-document
                 (archive-root-insert-sibling timed-document timed-heading
                                              star-count))))
      (let* ((current-parent-path
               (and parent-path
                    (edit-node-path timed-document timed-parent heading)))
             (updated-parent
               (and current-parent-path
                    (archive-node-at-path with-sibling-document
                                          current-parent-path))))
        (setf archive-sibling
              (find-if #'archive-sibling-heading-p
                       (archive-sibling-headings
                        (or updated-parent
                            (document-root with-sibling-document)))))))
    (unless archive-sibling
      (org-archive-signal-error :archive-sibling-not-found
                                "Could not create an archive sibling"))
    (let* ((current-heading (archive-node-at-path with-sibling-document
                                                  source-path))
           (relocation
             (refile-subtree with-sibling-document current-heading
                             with-sibling-document archive-sibling :mode :move))
           (final-document
             (edit-result-updated-document
              (relocation-result-target-edit relocation))))
      (archive-final-edit-result document final-document heading))))

;;;; Attachment directory resolution and write-plan builders.
(in-package #:org-kit)

(defun org-attachment-signal-error (reason message &optional context)
  (error 'org-attachment-error :reason reason :message message
         :context (and context (bounded-text context))))

(defun attachment-inheritance-enabled-p (heading)
  (multiple-value-bind (value present-p)
      (effective-heading-property-value heading "ATTACH_DIR_INHERIT")
    (and present-p (stringp value)
         (plusp (length (string-trim '(#\Space #\Tab) value)))
         (not (member (string-downcase
                       (string-trim '(#\Space #\Tab) value))
                      '("nil" "no" "false") :test #'string=)))))

(defun attachment-property-value (heading key)
  (let ((local (effective-heading-property-entries heading key)))
    (when local
      (return-from attachment-property-value
        (values (node-property (first local) :value) t)))
    (dolist (ancestor (effective-heading-ancestors heading))
      (when (attachment-inheritance-enabled-p ancestor)
        (let ((entries (effective-heading-property-entries ancestor key)))
          (when entries
            (return-from attachment-property-value
              (values (node-property (first entries) :value) t))))))
    (values nil nil)))

(defun attachment-source-directory (document source-directory)
  (let ((provided
          (typecase source-directory
            (string source-directory)
            (pathname (sb-ext:native-namestring source-directory))
            (null nil)
            (t (org-attachment-signal-error
                :invalid-source-directory
                "Source directory must be a path")))))
    (or provided
        (let ((source-name (document-source-name document)))
          (when source-name
            (let* ((source-path (pathname source-name))
                   (directory (pathname-directory source-path)))
              (and directory
                   (sb-ext:native-namestring
                    (make-pathname :directory directory :name nil :type nil
                                   :defaults source-path)))))))))

(defun attachment-absolute-path-p (path)
  (and (plusp (length path))
       (char= (char path 0) #\/)))

(defun attachment-check-path-character-code (code path)
  (when (or (<= #xd800 code #xdfff) (> code #x10ffff))
    (org-attachment-signal-error :invalid-path
                                 "Attachment path contains an invalid character"
                                 path)))

(defun attachment-check-path-byte-limit (path limits)
  (let ((byte-count 0))
    (loop for character across path
          for code = (char-code character)
          do (attachment-check-path-character-code code path)
             (incf byte-count
                   (cond ((<= code #x7f) 1)
                         ((<= code #x7ff) 2)
                         ((<= code #xffff) 3)
                         (t 4)))
             (check-limit limits :max-write-plan-path-bytes byte-count))
    byte-count))

(defun attachment-absolute-components (path)
  (unless (attachment-absolute-path-p path)
    (org-attachment-signal-error :invalid-path
                                 "Expected an absolute attachment path" path))
  (when (find #\\ path)
    (org-attachment-signal-error :invalid-path
                                 "Backslashes are not valid path separators" path))
  (let ((components nil)
        (start 1))
    (loop for end = (position #\/ path :start start)
          for component = (subseq path start end)
          do (cond
               ((or (zerop (length component)) (string= component ".")))
               ((string= component "..")
                (if components
                    (pop components)
                    (org-attachment-signal-error
                     :invalid-path "Attachment path escapes its root" path)))
               ((or (find #\Null component)
                    (find #\Newline component)
                    (find #\Return component))
                (org-attachment-signal-error
                 :invalid-path "Attachment path contains a control character" path))
               (t (push component components)))
          while end
          do (setf start (1+ end)))
    (nreverse components)))

(defun attachment-join-components (components)
  (format nil "~{~A~^/~}" components))

(defun attachment-relative-directory (path limits)
  (unless (stringp path)
    (org-attachment-signal-error :invalid-path
                                 "Attachment directory must be a string"))
  (when (find #\\ path)
    (org-attachment-signal-error :invalid-path
                                 "Backslashes are not valid path separators" path))
  (when (attachment-absolute-path-p path)
    (org-attachment-signal-error :absolute-path-requires-source-directory
                                 "Absolute paths require a source directory" path))
  (let ((trimmed (string-right-trim "/" path)))
    (cond
      ((string= trimmed ".") "")
      ((zerop (length trimmed))
       (org-attachment-signal-error :invalid-path
                                    "Attachment directory is empty" path))
      (t
       (handler-case
           (write-plan-normalized-path trimmed limits)
         (org-write-plan-error ()
           (org-attachment-signal-error :invalid-path
                                        "Attachment directory is invalid" path)))))))

(defun attachment-absolute-to-relative (path source-directory)
  (unless source-directory
    (org-attachment-signal-error
     :absolute-path-requires-source-directory
     "Absolute attachment paths require a source directory" path))
  (let* ((root (attachment-absolute-components source-directory))
         (components (attachment-absolute-components path)))
    (unless (and (<= (length root) (length components))
                 (equal root (subseq components 0 (length root))))
      (org-attachment-signal-error
       :path-outside-source-directory
       "Attachment path is outside the source directory" path))
    (attachment-join-components (nthcdr (length root) components))))

(defun attachment-resolve-directory (document path source-directory limits)
  (unless (and (stringp path) (plusp (length path)))
    (org-attachment-signal-error :invalid-path
                                 "Attachment directory must be non-empty" path))
  (if (attachment-absolute-path-p path)
      (let ((root (attachment-source-directory document source-directory)))
        (attachment-check-path-byte-limit path limits)
        (when root
          (attachment-check-path-byte-limit root limits))
        (let ((relative (attachment-absolute-to-relative path root)))
          (if (zerop (length relative))
              relative
              (attachment-relative-directory relative limits))))
      (attachment-relative-directory path limits)))

(defun attachment-id-directory (document id attachment-root source-directory)
  (unless (org-id-valid-value-p id)
    (org-attachment-signal-error :invalid-id
                                 "Attachment ID must be a non-empty string" id))
  (when (or (find #\/ id) (find #\\ id)
            (string= id ".") (string= id "..")
            (find #\Null id) (find #\Newline id) (find #\Return id))
    (org-attachment-signal-error :invalid-id
                                 "Attachment ID cannot be used as a path" id))
  (let* ((split (if (> (length id) 2)
                    (format nil "~A/~A" (subseq id 0 2) (subseq id 2))
                    (format nil "__/~A/~A" (subseq id 0 1) id)))
         (raw-root (or attachment-root "data"))
         (root (typecase raw-root
                 (string raw-root)
                 (pathname (sb-ext:native-namestring raw-root))
                 (t (org-attachment-signal-error
                     :invalid-attachment-root
                     "Attachment root must be a path"))))
         (combined
           (if (zerop (length root))
               split
               (format nil "~A/~A" (string-right-trim "/" root) split)))
         (limits (document-limits document)))
    (attachment-resolve-directory document combined source-directory limits)))

(defun resolve-attachment-directory
    (document heading &key attachment-root source-directory)
  "Resolve HEADING to a relative attachment directory for write plans."
  (unless (document-p document)
    (org-attachment-signal-error :invalid-document
                                 "Expected an Org document"))
  (let* ((headline (edit-resolve-node document heading '(:headline)))
         (limits (document-limits document))
         (source-directory
           (attachment-source-directory document source-directory)))
    (multiple-value-bind (directory present-p)
        (attachment-property-value headline "DIR")
      (when present-p
        (return-from resolve-attachment-directory
          (attachment-resolve-directory document directory source-directory
                                        limits))))
    (multiple-value-bind (directory present-p)
        (attachment-property-value headline "ATTACH_DIR")
      (when present-p
        (return-from resolve-attachment-directory
          (attachment-resolve-directory document directory source-directory
                                        limits))))
    (multiple-value-bind (id present-p)
        (attachment-property-value headline "ID")
      (unless present-p
        (org-attachment-signal-error
         :missing-attachment-location
         "Heading has neither DIR nor ID attachment properties"))
      (attachment-id-directory document id attachment-root source-directory))))

(defun attachment-validate-filename (filename)
  (unless (and (stringp filename) (plusp (length filename))
               (not (member filename '("." "..") :test #'string=))
               (not (find #\/ filename))
               (not (find #\\ filename))
               (not (find #\Null filename))
               (not (find #\Newline filename))
               (not (find #\Return filename)))
    (org-attachment-signal-error :invalid-filename
                                 "Attachment filename must be one path component"
                                 (and (stringp filename) filename)))
  filename)

(defun attachment-entry-path (directory filename limits)
  (let ((path (if (zerop (length directory))
                  filename
                  (format nil "~A/~A" directory filename))))
    (handler-case
        (write-plan-normalized-path path limits)
      (org-write-plan-error ()
        (org-attachment-signal-error :invalid-path
                                     "Attachment path is invalid" path)))))

(defun attachment-check-plan-entry (path limits content)
  (check-limit limits :max-write-plan-entries 1)
  (multiple-value-bind (normalized components)
      (write-plan-normalized-path path limits)
    (declare (ignore normalized components)))
  (when content
    (if (stringp content)
        (write-plan-string-byte-count content path limits 0)
        (check-limit limits :max-write-plan-bytes (length content))))
  path)

(defun add-attachment
    (document heading filename content &key attachment-root source-directory)
  "Return a write plan that writes one attachment without applying it."
  (unless (document-p document)
    (org-attachment-signal-error :invalid-document
                                 "Expected an Org document"))
  (unless (or (stringp content)
              (typep content '(vector (unsigned-byte 8))))
    (error 'type-error :datum content
           :expected-type '(or string (vector (unsigned-byte 8)))))
  (let* ((limits (document-limits document))
         (directory
           (resolve-attachment-directory
            document heading :attachment-root attachment-root
            :source-directory source-directory))
         (path (attachment-entry-path directory
                                     (attachment-validate-filename filename)
                                     limits)))
    (attachment-check-plan-entry path limits content)
    (make-write-plan
     (list (make-write-plan-entry path content :create-directories-p t)))))

(defun delete-attachment
    (document heading filename &key attachment-root source-directory)
  "Return a write plan that deletes one attachment without applying it."
  (unless (document-p document)
    (org-attachment-signal-error :invalid-document
                                 "Expected an Org document"))
  (let* ((limits (document-limits document))
         (directory
           (resolve-attachment-directory
            document heading :attachment-root attachment-root
            :source-directory source-directory))
         (path (attachment-entry-path directory
                                     (attachment-validate-filename filename)
                                     limits)))
    (attachment-check-plan-entry path limits nil)
    (make-write-plan (list (make-write-plan-delete-entry path)))))

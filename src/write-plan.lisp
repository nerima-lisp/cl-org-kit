;;;; src/write-plan.lisp
(in-package #:org-kit)

(sb-alien:define-alien-routine ("openat" %write-plan-openat) sb-alien:int
  (directory-fd sb-alien:int)
  (path sb-alien:c-string)
  (flags sb-alien:int)
  (mode sb-alien:unsigned-int))

(sb-alien:define-alien-routine ("mkdirat" %write-plan-mkdirat) sb-alien:int
  (directory-fd sb-alien:int)
  (path sb-alien:c-string)
  (mode sb-alien:unsigned-int))

(sb-alien:define-alien-routine ("renameat" %write-plan-renameat) sb-alien:int
  (old-directory-fd sb-alien:int)
  (old-path sb-alien:c-string)
  (new-directory-fd sb-alien:int)
  (new-path sb-alien:c-string))

(sb-alien:define-alien-routine ("unlinkat" %write-plan-unlinkat) sb-alien:int
  (directory-fd sb-alien:int)
  (path sb-alien:c-string)
  (flags sb-alien:int))

(defstruct (write-plan-entry
            (:constructor %make-write-plan-entry)
            (:conc-name %write-plan-entry-)
            (:copier nil)
            (:predicate %write-plan-entry-p))
  "One relative text or byte file to create or replace."
  (path "" :read-only t)
  (content "" :read-only t)
  (mode #o644 :read-only t)
  (create-directories-p nil :read-only t))

(defun make-write-plan-entry (path content &key (mode #o644) create-directories-p)
  "Return a write-plan entry without touching the filesystem.
PATH is relative. String CONTENT is encoded as UTF-8; an octet vector is copied
and written unchanged. MODE defaults to 0644, and CREATE-DIRECTORIES-P controls
creation of missing parent directories."
  (unless (or (stringp path) (pathnamep path))
    (error 'type-error :datum path :expected-type '(or string pathname)))
  (unless (or (stringp content)
              (typep content '(vector (unsigned-byte 8))))
    (error 'type-error :datum content
           :expected-type '(or string (vector (unsigned-byte 8)))))
  (unless (typep mode '(integer 0 #o777))
    (error 'type-error :datum mode :expected-type '(integer 0 #o777)))
  (unless (member create-directories-p '(nil t))
    (error 'type-error :datum create-directories-p :expected-type 'boolean))
  (%make-write-plan-entry :path (copy-seq (if (pathnamep path)
                                               (sb-ext:native-namestring path)
                                               path))
                          :content (copy-seq content)
                          :mode mode
                          :create-directories-p create-directories-p))

(defun write-plan-entry-p (object)
  "Return true when OBJECT is a write-plan entry."
  (%write-plan-entry-p object))

(defun write-plan-entry-path (entry)
  "Return a copy of ENTRY's relative path."
  (copy-seq (%write-plan-entry-path entry)))

(defun write-plan-entry-content (entry)
  "Return a copy of ENTRY's text string or octet-vector content."
  (copy-seq (%write-plan-entry-content entry)))

(defun write-plan-entry-mode (entry)
  "Return ENTRY's permission bits."
  (%write-plan-entry-mode entry))

(defun write-plan-entry-create-directories-p (entry)
  "Return true when ENTRY permits creation of missing parent directories."
  (%write-plan-entry-create-directories-p entry))

(defstruct (write-plan
            (:constructor %make-write-plan)
            (:conc-name %write-plan-)
            (:copier nil)
            (:predicate %write-plan-p))
  "An ordered sequence of caller-created write-plan entries."
  (entries #() :read-only t))

(defun make-write-plan (entries)
  "Return a write plan from a finite list or vector of write-plan entries."
  (let ((vector
          (typecase entries
            (vector (copy-seq entries))
            (null #())
            (cons (let ((length (list-length entries)))
                    (unless length
                      (error 'type-error :datum entries
                             :expected-type '(or null list vector)))
                    (coerce entries 'vector)))
            (t (error 'type-error :datum entries
                      :expected-type '(or null list vector))))))
    (unless (every #'write-plan-entry-p vector)
      (error 'type-error :datum entries
             :expected-type '(sequence write-plan-entry)))
    (%make-write-plan :entries vector)))

(defun write-plan-p (object)
  "Return true when OBJECT is a write plan."
  (%write-plan-p object))

(defun write-plan-entries (plan)
  "Return a copy of PLAN's entry vector."
  (copy-seq (%write-plan-entries plan)))

(defun write-plan-error (reason &optional path cause)
  (error 'org-write-plan-error
         :message "Write plan rejected"
         :path (and path (bounded-text path))
         :reason reason
         :cause cause))

(defun write-plan-split-path (path)
  (when (or (zerop (length path))
            (char= (char path 0) #\/)
            (char= (char path (1- (length path))) #\/)
            (position #\Null path))
    (write-plan-error (if (and (> (length path) 0)
                               (char= (char path 0) #\/))
                          :absolute-path
                          :invalid-path)
                      path))
  (let ((parts '())
        (start 0))
    (loop for end = (position #\/ path :start start)
          for part = (subseq path start end)
          do (cond ((string= part "..")
                    (write-plan-error :parent-component path))
                   ((or (zerop (length part)) (string= part ".")))
                   (t (push part parts)))
          while end
          do (setf start (1+ end)))
    (unless parts
      (write-plan-error :invalid-path path))
    (nreverse parts)))

(defun write-plan-normalized-path (path)
  (let* ((raw (if (pathnamep path) (sb-ext:native-namestring path) path))
         (components (write-plan-split-path raw)))
    (values (format nil "~{~A~^/~}" components) components)))

(defun write-plan-check-path-conflicts (records)
  (let ((paths (make-hash-table :test 'equal))
        (prefixes (make-hash-table :test 'equal)))
    (dolist (record records)
      (let ((path (second record))
            (key (fifth record))
            (components (sixth record)))
        (when (gethash key paths)
          (write-plan-error :duplicate-path path))
        (let ((node prefixes))
          (dolist (component components)
            (when (gethash :file node)
              (write-plan-error :path-conflict path))
            (setf node
                  (or (gethash component node)
                      (setf (gethash component node)
                            (make-hash-table :test 'equal)))))
          (when (plusp (hash-table-count node))
            (write-plan-error :path-conflict path))
          (setf (gethash :file node) t))
        (setf (gethash key paths) t)))
    records))

(defun write-plan-canonical-root (root)
  (unless (or (stringp root) (pathnamep root))
    (error 'type-error :datum root :expected-type '(or string pathname)))
  (handler-case
      (let* ((pathname (if (stringp root)
                           (sb-ext:parse-native-namestring root)
                           root))
             (canonical (sb-ext:native-namestring (truename pathname)))
             (stat (sb-posix:lstat canonical))
             (kind (logand (sb-posix:stat-mode stat) #o170000)))
        (unless (= kind #o040000)
          (write-plan-error :root-not-directory canonical))
        (if (char= (char canonical (1- (length canonical))) #\/)
            canonical
            (concatenate 'string canonical "/")))
    (org-write-plan-error (condition) (error condition))
    (sb-posix:syscall-error (condition)
      (write-plan-error :io-failed (princ-to-string root) condition))
    (error () (write-plan-error :invalid-root (princ-to-string root)))))

(defun write-plan-lstat (path)
  (handler-case
      (sb-posix:lstat path)
    (sb-posix:syscall-error (condition)
      (if (= (sb-posix:syscall-errno condition) sb-posix:enoent)
          nil
          (write-plan-error :io-failed path condition)))))

(defun write-plan-path-type (stat)
  (logand (sb-posix:stat-mode stat) #o170000))

(defun write-plan-inspect-entry (root path components create-directories-p)
  (let ((current root)
        (last-index (1- (length components))))
    (loop for component in components
          for index from 0
          for final-p = (= index last-index)
          do (setf current (concatenate 'string current
                                        (if (char= (char current
                                                        (1- (length current)))
                                                   #\/) "" "/")
                                        component))
             (let ((stat (write-plan-lstat current)))
               (if stat
                   (let ((kind (write-plan-path-type stat)))
                     (when (= kind #o120000)
                       (write-plan-error :symlink-component path))
                     (if final-p
                         (unless (= kind #o100000)
                           (write-plan-error :target-not-regular-file path))
                         (unless (= kind #o040000)
                           (write-plan-error :parent-not-directory path))))
                   (unless (or final-p create-directories-p)
                     (write-plan-error :missing-parent-directory path)))))
    t))

(defun write-plan-string-byte-count (string path limits total)
  ;; The codec's size helper encodes its input, so bound its temporary octets.
  (let ((byte-count 0)
        (chunk-size 4096))
    (loop for start from 0 below (length string) by chunk-size
          for end = (min (length string) (+ start chunk-size))
          for chunk-byte-count =
            (handler-case
                (cl-codec-kit:string-size-in-octets
                 string :start start :end end :encoding :utf-8)
              (error () (write-plan-error :invalid-utf-8 path)))
          do (incf byte-count chunk-byte-count)
             (check-limit limits :max-write-plan-bytes (+ total byte-count)))
    byte-count))

(defun write-plan-encode-string (string path)
  (handler-case
      (cl-codec-kit:string-to-octets string :encoding :utf-8)
    (error () (write-plan-error :invalid-utf-8 path))))

(defun write-plan-encode-entry (entry index total limits)
  (let* ((content (%write-plan-entry-content entry))
         (path (%write-plan-entry-path entry))
         (byte-count (if (stringp content)
                         (write-plan-string-byte-count content path limits total)
                         (length content)))
         (next-total (+ total byte-count)))
    (check-limit limits :max-write-plan-bytes next-total)
    (let ((bytes
            (if (stringp content)
                (write-plan-encode-string content path)
                (copy-seq content))))
      (unless (= (length bytes) byte-count)
        (write-plan-error :utf-8-size-mismatch path))
      (list entry index bytes next-total))))

(defun write-plan-prepare (plan root limits)
  (unless (write-plan-p plan)
    (error 'type-error :datum plan :expected-type 'write-plan))
  (unless (limits-p limits)
    (error 'type-error :datum limits :expected-type 'limits))
  (let* ((entries (%write-plan-entries plan))
         (count (length entries)))
    (check-limit limits :max-write-plan-entries count)
    (let ((records '())
          (total 0))
      (loop for entry across entries
            for index from 0
            do (multiple-value-bind (path components)
                   (write-plan-normalized-path (%write-plan-entry-path entry))
                 (let* ((key (string-downcase path))
                        (key-components (write-plan-split-path key))
                        (encoded (write-plan-encode-entry entry index total limits))
                        (next-total (fourth encoded)))
                   (setf total next-total)
                   (push (list entry path components (third encoded)
                               key key-components)
                         records))))
      (setf records (nreverse records))
      (write-plan-check-path-conflicts records)
      (let ((canonical-root (write-plan-canonical-root root)))
        (dolist (record records)
          (write-plan-inspect-entry canonical-root (second record) (third record)
                                    (%write-plan-entry-create-directories-p
                                     (first record))))
        (values canonical-root records)))))

(defun write-plan-syscall-result (result name)
  (if (= result -1)
      (error 'sb-posix:syscall-error
             :errno (sb-alien:get-errno)
             :name name)
      result))

(defun write-plan-openat (directory-fd path flags &optional (mode 0))
  (write-plan-syscall-result
   (%write-plan-openat directory-fd path flags mode)
   "openat"))

(defun write-plan-root-components (root)
  (let ((start 1)
        (components '()))
    (loop for separator = (position #\/ root :start start)
          for end = (or separator (length root))
          for component = (subseq root start end)
          unless (zerop (length component))
            do (push component components)
          while separator
            do (setf start (1+ separator)))
    (nreverse components)))

(defun write-plan-open-root-directory (root)
  (let ((root-fd (sb-posix:open "/"
                                (logior sb-posix:o-rdonly
                                        sb-posix:o-directory
                                        sb-posix:o-nofollow))))
    (handler-case
        (let ((directory-fds (list root-fd)))
          (unwind-protect
               (progn
                 (dolist (component (write-plan-root-components root))
                   (push (write-plan-openat
                          (car directory-fds) component
                          (logior sb-posix:o-rdonly
                                  sb-posix:o-directory
                                  sb-posix:o-nofollow))
                         directory-fds))
                 (loop while (cdr directory-fds)
                       do (sb-posix:close (pop (cdr directory-fds))))
                 (prog1 (car directory-fds)
                   (setf directory-fds nil)))
            (dolist (fd directory-fds)
              (ignore-errors (sb-posix:close fd)))))
      (sb-posix:syscall-error (condition)
        (write-plan-error :io-failed root condition)))))

(defun write-plan-open-child-directory (directory-fd component path create-p)
  (handler-case
      (write-plan-openat directory-fd component
                         (logior sb-posix:o-rdonly
                                 sb-posix:o-directory
                                 sb-posix:o-nofollow))
    (sb-posix:syscall-error (condition)
      (if (= (sb-posix:syscall-errno condition) sb-posix:enoent)
          (if create-p
              (progn
                (handler-case
                    (write-plan-syscall-result
                     (%write-plan-mkdirat directory-fd component #o755)
                     "mkdirat")
                  (sb-posix:syscall-error (mkdir-condition)
                    (unless (= (sb-posix:syscall-errno mkdir-condition)
                               sb-posix:eexist)
                      (error mkdir-condition))))
                (write-plan-openat directory-fd component
                                   (logior sb-posix:o-rdonly
                                           sb-posix:o-directory
                                           sb-posix:o-nofollow)))
              (write-plan-error :missing-parent-directory path))
          (error condition)))))

(defun write-plan-open-parent-directory (root-fd record)
  (let ((directory-fds (list (sb-posix:dup root-fd)))
        (path (second record))
        (create-p (%write-plan-entry-create-directories-p (first record))))
    (unwind-protect
         (progn
           (dolist (component (butlast (third record)))
             (push (write-plan-open-child-directory
                    (car directory-fds) component path create-p)
                   directory-fds))
           (loop while (cdr directory-fds)
                 do (sb-posix:close (pop (cdr directory-fds))))
           (prog1 (car directory-fds)
             (setf directory-fds nil)))
      (dolist (fd directory-fds)
        (ignore-errors (sb-posix:close fd))))))

(defun write-plan-random-temporary-name (random-state)
  (concatenate 'string ".org-kit-"
               (format nil "~36R" (random most-positive-fixnum random-state))
               ".tmp"))

(defun write-plan-open-temporary-file (parent-fd path random-state)
  (loop repeat 100
        for name = (write-plan-random-temporary-name random-state)
        do (handler-case
               (return
                 (values name
                         (write-plan-openat
                          parent-fd name
                          (logior sb-posix:o-creat
                                  sb-posix:o-excl
                                  sb-posix:o-wronly
                                  sb-posix:o-nofollow)
                          #o600)))
             (sb-posix:syscall-error (condition)
               (unless (= (sb-posix:syscall-errno condition) sb-posix:eexist)
                 (error condition))))
        finally (write-plan-error :temporary-name-exhausted path)))

(defun write-plan-rename-temporary-file (parent-fd temporary-name target-name path)
  (declare (ignore path))
  (write-plan-syscall-result
   (%write-plan-renameat parent-fd temporary-name parent-fd target-name)
   "renameat"))

(defun write-plan-apply-record (root-fd record random-state)
  (let ((path (second record))
        (parent-fd nil))
    (handler-case
        (progn
          (setf parent-fd (write-plan-open-parent-directory root-fd record))
          (let ((completed-p nil))
            (unwind-protect
                 (progn
                   (multiple-value-bind (temporary-name fd)
                       (write-plan-open-temporary-file parent-fd path random-state)
                     (let ((stream nil)
                           (fd-open-p t)
                           (stream-closed-p nil)
                           (renamed-p nil))
                       (unwind-protect
                            (progn
                              (setf stream
                                    (sb-sys:make-fd-stream
                                     fd :output t
                                     :element-type '(unsigned-byte 8)
                                     :name temporary-name))
                              (write-sequence (fourth record) stream)
                              (finish-output stream)
                              (sb-posix:fchmod fd
                                               (%write-plan-entry-mode
                                                (first record)))
                              (sb-posix:fsync fd)
                              (close stream)
                              (setf stream-closed-p t
                                    fd-open-p nil)
                              (write-plan-rename-temporary-file
                               parent-fd temporary-name
                               (car (last (third record))) path)
                              (setf renamed-p t))
                         (unless stream-closed-p
                           (if stream
                               (ignore-errors (close stream :abort t))
                               (when fd-open-p
                                 (ignore-errors (sb-posix:close fd)))))
                         (unless renamed-p
                           (ignore-errors
                             (write-plan-syscall-result
                              (%write-plan-unlinkat parent-fd temporary-name 0)
                              "unlinkat")))))
                   (setf completed-p t))
              (if completed-p
                  (sb-posix:close parent-fd)
                  (ignore-errors (sb-posix:close parent-fd)))))
          path))
      (org-write-plan-error (condition)
        (error condition))
      (error (condition)
        (write-plan-error :io-failed path condition)))))

(defun apply-write-plan (plan root &key (limits (make-limits)))
  "Validate PLAN in full, then atomically replace each file below ROOT.
All paths are relative POSIX paths. Every entry and limit is checked before
directory or file creation. Each file uses a same-directory temporary file and
rename; a multi-file plan is not transactional."
  (multiple-value-bind (canonical-root records)
      (write-plan-prepare plan root limits)
    (handler-case
        (let ((root-fd (write-plan-open-root-directory canonical-root))
              (random-state (sb-ext:seed-random-state t))
              (completed-p nil))
          (unwind-protect
               (prog1 (mapcar (lambda (record)
                                (write-plan-apply-record root-fd record
                                                         random-state))
                              records)
                 (setf completed-p t))
            (if completed-p
                (sb-posix:close root-fd)
                (ignore-errors (sb-posix:close root-fd)))))
      (org-write-plan-error (condition)
        (error condition))
      (error (condition)
        (write-plan-error :io-failed canonical-root condition)))))

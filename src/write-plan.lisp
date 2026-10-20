;;;; src/write-plan.lisp
(in-package #:org-kit)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun write-plan-symbol-value (symbol)
    (and symbol (boundp symbol) (symbol-value symbol))))

(defconstant +write-plan-f-dupfd-cloexec+
  (or (let ((symbol (find-symbol "F-DUPFD-CLOEXEC" :sb-posix)))
        (write-plan-symbol-value symbol))
      #+darwin 67
      #+linux 1030
      (error "F_DUPFD_CLOEXEC is not defined for this platform.")))

(defconstant +write-plan-o-cloexec+
  (or (let ((symbol (find-symbol "O-CLOEXEC" :sb-posix)))
        (write-plan-symbol-value symbol))
      #+darwin #x01000000
      #+linux #x00080000
      (error "O_CLOEXEC is not defined for this platform.")))

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
  (mode-supplied-p nil :read-only t)
  (create-directories-p nil :read-only t))

(defun make-write-plan-entry
    (path content &key (mode #o644 mode-supplied-p) create-directories-p)
  "Return a write-plan entry without touching the filesystem.
PATH is relative. String CONTENT is encoded as UTF-8; an octet vector is copied
and written unchanged. If MODE is omitted, an existing regular file's mode is
preserved and a new file uses 0644. An explicit MODE is applied exactly,
regardless of the process umask. CREATE-DIRECTORIES-P controls creation of
missing parent directories."
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
                          :mode-supplied-p mode-supplied-p
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

(defun write-plan-split-path (path &optional (limits (make-limits)))
  (let ((byte-count 0)
        (depth 0)
        (component-length 0)
        (component-dot-only-p t)
        (max-path-bytes (limit-value limits :max-write-plan-path-bytes))
        (max-path-depth (limit-value limits :max-write-plan-path-depth)))
    (labels ((finish-component ()
               (when (and (plusp component-length)
                          (or (/= component-length 1)
                              (not component-dot-only-p)))
                 (incf depth)
                 (when (> depth max-path-depth)
                   (check-limit limits :max-write-plan-path-depth depth)))
               (setf component-length 0
                     component-dot-only-p t)))
      (loop for character across path
            for code = (char-code character)
            do (when (or (<= #xd800 code #xdfff) (> code #x10ffff))
                 (write-plan-error :invalid-path path))
               (incf byte-count
                     (cond ((<= code #x7f) 1)
                           ((<= code #x7ff) 2)
                           ((<= code #xffff) 3)
                           (t 4)))
               (when (> byte-count max-path-bytes)
                 (check-limit limits :max-write-plan-path-bytes byte-count))
               (if (char= character #\/)
                   (finish-component)
                   (progn
                     (incf component-length)
                     (unless (char= character #\.)
                       (setf component-dot-only-p nil)))))
      (finish-component))
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
      (nreverse parts))))

(defun write-plan-normalized-path (path &optional (limits (make-limits)))
  (let* ((raw (if (pathnamep path) (sb-ext:native-namestring path) path))
         (components (write-plan-split-path raw limits)))
    (values (format nil "~{~A~^/~}" components) components)))

(defun write-plan-conflict-key (string)
  (sb-unicode:normalize-string (sb-unicode:casefold string) :nfc))

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
        canonical)
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
  (let ((directory-fds nil)
        (existing-mode nil)
        (missing-parent-p nil))
    (labels ((path-for-components (path-components)
               (with-output-to-string (stream)
                 (write-string root stream)
                 (let ((separator-needed-p
                         (not (char= (char root (1- (length root))) #\/))))
                   (dolist (component path-components)
                     (when separator-needed-p
                       (write-char #\/ stream))
                     (write-string component stream)
                     (setf separator-needed-p t)))))
             (open-parent (directory-fd component index)
               (handler-case
                   (write-plan-openat
                    directory-fd component
                    (logior sb-posix:o-rdonly
                            sb-posix:o-directory
                            sb-posix:o-nofollow))
                 (sb-posix:syscall-error (condition)
                   (let ((errno (sb-posix:syscall-errno condition)))
                     (cond
                       ((= errno sb-posix:enoent) nil)
                       ((= errno sb-posix:eloop)
                        (write-plan-error :symlink-component path condition))
                       ((= errno sb-posix:enotdir)
                        (let ((stat
                                (write-plan-lstat
                                 (path-for-components
                                  (subseq components 0 (1+ index))))))
                          (if (and stat
                                   (= (write-plan-path-type stat) #o120000))
                              (write-plan-error :symlink-component path condition)
                              (write-plan-error :parent-not-directory path
                                                condition))))
                       (t (write-plan-error :io-failed path condition))))))))
      (unwind-protect
           (progn
             (sb-sys:without-interrupts
               (setf directory-fds (list (write-plan-open-root-directory root))))
             (loop for component in (butlast components)
                   for index from 0
                   do (let ((child-fd nil))
                        (sb-sys:without-interrupts
                          (setf child-fd
                                (open-parent (car directory-fds)
                                             component index))
                          (when child-fd
                            (push child-fd directory-fds)))
                        (unless child-fd
                          (if create-directories-p
                              (progn
                                (setf missing-parent-p t)
                                (return))
                              (write-plan-error :missing-parent-directory path)))))
             (unless missing-parent-p
               (let ((stat (write-plan-lstat
                            (path-for-components components))))
                 (when stat
                   (let ((kind (write-plan-path-type stat)))
                     (when (= kind #o120000)
                       (write-plan-error :symlink-component path))
                     (unless (= kind #o100000)
                       (write-plan-error :target-not-regular-file path))
                     (setf existing-mode
                           (logand (sb-posix:stat-mode stat) #o777))))))
             existing-mode)
        (sb-sys:without-interrupts
          (dolist (directory-fd directory-fds)
            (ignore-errors (sb-posix:close directory-fd))))))))

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
                   (write-plan-normalized-path (%write-plan-entry-path entry)
                                               limits)
                 (let* ((key (write-plan-conflict-key path))
                        (key-components (mapcar #'write-plan-conflict-key
                                                components))
                        (encoded (write-plan-encode-entry entry index total limits))
                        (next-total (fourth encoded)))
                   (setf total next-total)
                   (push (list entry path components (third encoded)
                               key key-components nil)
                         records))))
      (setf records (nreverse records))
      (write-plan-check-path-conflicts records)
      (let ((canonical-root (write-plan-canonical-root root)))
        (dolist (record records)
          (setf (seventh record)
                (write-plan-inspect-entry canonical-root (second record)
                                          (third record)
                                          (%write-plan-entry-create-directories-p
                                           (first record)))))
        (values canonical-root records)))))

(defun write-plan-syscall-result (result name)
  (if (= result -1)
      (error 'sb-posix:syscall-error
             :errno (sb-alien:get-errno)
             :name name)
      result))

(defun write-plan-openat (directory-fd path flags &optional (mode 0))
  (write-plan-syscall-result
   (%write-plan-openat directory-fd path
                       (logior flags +write-plan-o-cloexec+) mode)
   "openat"))

(defun write-plan-duplicate-close-on-exec (fd)
  (sb-posix:fcntl fd +write-plan-f-dupfd-cloexec+ 0))

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
  (let ((directory-fds nil))
    (unwind-protect
         (handler-case
             (progn
               (sb-sys:without-interrupts
                 (push (sb-posix:open "/"
                                     (logior sb-posix:o-rdonly
                                             sb-posix:o-directory
                                             sb-posix:o-nofollow
                                             +write-plan-o-cloexec+))
                       directory-fds))
               (dolist (component (write-plan-root-components root))
                 (sb-sys:without-interrupts
                   (push (write-plan-openat
                          (car directory-fds) component
                          (logior sb-posix:o-rdonly
                                  sb-posix:o-directory
                                  sb-posix:o-nofollow))
                         directory-fds)))
               (loop while (cdr directory-fds)
                     do (sb-sys:without-interrupts
                          (sb-posix:close (cadr directory-fds))
                          (pop (cdr directory-fds))))
               (prog1 (car directory-fds)
                 (setf directory-fds nil)))
           (sb-posix:syscall-error (condition)
             (write-plan-error :io-failed root condition)))
      (sb-sys:without-interrupts
        (dolist (fd directory-fds)
          (ignore-errors (sb-posix:close fd)))))))

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

(defun write-plan-open-parent-directory
    (root-fd record &optional
               (create-p
                 (%write-plan-entry-create-directories-p (first record))))
  (let ((directory-fds nil)
        (path (second record)))
    (unwind-protect
         (progn
           (sb-sys:without-interrupts
             (push (write-plan-duplicate-close-on-exec root-fd)
                   directory-fds))
           (dolist (component (butlast (third record)))
             (sb-sys:without-interrupts
               (push (write-plan-open-child-directory
                      (car directory-fds) component path create-p)
                     directory-fds)))
           (loop while (cdr directory-fds)
                 do (sb-sys:without-interrupts
                      (sb-posix:close (cadr directory-fds))
                      (pop (cdr directory-fds))))
           (prog1 (car directory-fds)
             (setf directory-fds nil)))
      (sb-sys:without-interrupts
        (dolist (fd directory-fds)
          (ignore-errors (sb-posix:close fd)))))))

(defun write-plan-verify-parent-directory (root-fd record parent-fd)
  (let ((path (second record))
        (current-fd nil))
    (unwind-protect
         (progn
           (setf current-fd
                 (handler-case
                     (write-plan-open-parent-directory root-fd record nil)
                   (error (condition)
                     (write-plan-error :containment-lost path condition))))
           (let* ((opened-stat
                    (handler-case
                        (sb-posix:fstat parent-fd)
                      (error (condition)
                        (write-plan-error :containment-lost path condition))))
                  (current-stat
                    (handler-case
                        (sb-posix:fstat current-fd)
                      (error (condition)
                        (write-plan-error :containment-lost path condition))))
                  (same-directory-p
                    (and (= (sb-posix:stat-dev opened-stat)
                            (sb-posix:stat-dev current-stat))
                         (= (sb-posix:stat-ino opened-stat)
                            (sb-posix:stat-ino current-stat)))))
             (unless same-directory-p
               (write-plan-error :containment-lost path))
             t))
      (when current-fd
        (sb-sys:without-interrupts
          (ignore-errors (sb-posix:close current-fd)))))))

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
    (unwind-protect
         (handler-case
             (progn
               (sb-sys:without-interrupts
                 (setf parent-fd
                       (write-plan-open-parent-directory root-fd record)))
               (let ((temporary-name nil)
                     (fd nil)
                     (stream nil)
                     (fd-open-p nil)
                     (stream-closed-p nil)
                     (renamed-p nil))
                 (unwind-protect
                      (progn
                        (sb-sys:without-interrupts
                          (multiple-value-setq (temporary-name fd)
                            (write-plan-open-temporary-file
                             parent-fd path random-state))
                          (setf fd-open-p t))
                        (sb-sys:without-interrupts
                          (setf stream
                                (sb-sys:make-fd-stream
                                 fd :output t
                                 :element-type '(unsigned-byte 8)
                                 :name temporary-name)
                                fd-open-p nil))
                        (write-sequence (fourth record) stream)
                        (finish-output stream)
                        (sb-posix:fchmod
                         fd
                         (if (%write-plan-entry-mode-supplied-p
                              (first record))
                             (%write-plan-entry-mode (first record))
                             (or (seventh record) #o644)))
                        (sb-posix:fsync fd)
                        (sb-sys:without-interrupts
                          (close stream)
                          (setf stream-closed-p t))
                        (write-plan-rename-temporary-file
                         parent-fd temporary-name
                         (car (last (third record))) path)
                        (setf renamed-p t))
                   (unless stream-closed-p
                     (if stream
                         (sb-sys:without-interrupts
                           (ignore-errors (close stream :abort t)))
                         (when fd-open-p
                           (sb-sys:without-interrupts
                             (ignore-errors (sb-posix:close fd))
                             (setf fd-open-p nil)))))
                   (unless renamed-p
                     (ignore-errors
                       (write-plan-syscall-result
                        (%write-plan-unlinkat
                         parent-fd temporary-name 0)
                        "unlinkat")))))
               (write-plan-verify-parent-directory root-fd record parent-fd)
               (sb-sys:without-interrupts
                 (sb-posix:close parent-fd)
                 (setf parent-fd nil))
               path)
           (org-write-plan-error (condition)
             (error condition))
           (error (condition)
             (write-plan-error :io-failed path condition)))
      (when parent-fd
        (sb-sys:without-interrupts
          (ignore-errors (sb-posix:close parent-fd))
          (setf parent-fd nil))))))

(defun apply-write-plan (plan root &key (limits (make-limits)))
  "Validate PLAN in full, then replace each target below ROOT through a
same-directory temporary file and rename. ROOT must name an existing
directory; a symlink used as ROOT resolves to its target. All paths are relative
POSIX paths. Every entry and limit is checked before directory or file creation.
Returns normalized relative paths in plan order; a multi-file plan is not
transactional. The caller must prevent other actors from renaming or replacing
path components during application. The library rejects absolute paths, `..`,
NUL, and symlink components below the resolved root. After each write, it
compares the opened parent's device and inode with a root-relative re-open. That
check reports a missing or different parent. A path restored before the check
is not detected. The check cannot prevent a rename during a write or after the
check."
  (multiple-value-bind (canonical-root records)
      (write-plan-prepare plan root limits)
    (handler-case
        (let ((root-fd nil)
              (random-state (sb-ext:seed-random-state t)))
          (unwind-protect
               (progn
                 (sb-sys:without-interrupts
                   (setf root-fd
                         (write-plan-open-root-directory canonical-root)))
                 (mapcar (lambda (record)
                           (write-plan-apply-record root-fd record
                                                    random-state))
                         records))
            (sb-sys:without-interrupts
              (when root-fd
                (ignore-errors (sb-posix:close root-fd))
                (setf root-fd nil)))))
      (org-write-plan-error (condition)
        (error condition))
      (error (condition)
        (write-plan-error :io-failed canonical-root condition)))))

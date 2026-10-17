;;;; src/write-plan.lisp
(in-package #:org-kit)

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
                                               (namestring path)
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

(defun write-plan-error (reason &optional path)
  (error 'org-write-plan-error
         :message "Write plan rejected"
         :path (and path (bounded-text path))
         :reason reason))

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
  (let* ((raw (if (pathnamep path) (namestring path) path))
         (components (write-plan-split-path raw)))
    (values (format nil "~{~A~^/~}" components) components)))

(defun write-plan-conflict-reason (left right)
  (let ((left (string-downcase left))
        (right (string-downcase right)))
    (cond ((string= left right) :duplicate-path)
          ((and (< (length left) (length right))
                (string= left right :end1 (length left) :end2 (length left))
                (char= (char right (length left)) #\/))
           :path-conflict)
          ((and (< (length right) (length left))
                (string= right left :end1 (length right) :end2 (length right))
                (char= (char left (length right)) #\/))
           :path-conflict))))

(defun write-plan-check-path-conflicts (records)
  (loop for tail on records
        for path = (second (first tail))
        do (dolist (other (rest tail))
             (let ((reason (write-plan-conflict-reason path (second other))))
               (when reason (write-plan-error reason path)))))
  records)

(defun write-plan-canonical-root (root)
  (unless (or (stringp root) (pathnamep root))
    (error 'type-error :datum root :expected-type '(or string pathname)))
  (handler-case
      (let* ((canonical (namestring (truename root)))
             (stat (sb-posix:lstat canonical))
             (kind (logand (sb-posix:stat-mode stat) #o170000)))
        (unless (= kind #o040000)
          (write-plan-error :root-not-directory canonical))
        (if (char= (char canonical (1- (length canonical))) #\/)
            canonical
            (concatenate 'string canonical "/")))
    (org-write-plan-error (condition) (error condition))
    (error () (write-plan-error :invalid-root (princ-to-string root)))))

(defun write-plan-lstat (path)
  (handler-case
      (sb-posix:lstat path)
    (sb-posix:syscall-error (condition)
      (if (= (sb-posix:syscall-errno condition) sb-posix:enoent)
          nil
          (write-plan-error :path-inspection-failed path)))))

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

(defun write-plan-string-byte-count (string path)
  (loop for character across string
        for code = (char-code character)
        sum (progn
              (when (or (> code #x10FFFF)
                        (<= #xD800 code #xDFFF))
                (write-plan-error :invalid-utf-8 path))
              (cond ((<= code #x7F) 1)
                    ((<= code #x7FF) 2)
                    ((<= code #xFFFF) 3)
                    (t 4)))))

(defun write-plan-encode-entry (entry index total limits)
  (let* ((content (%write-plan-entry-content entry))
         (path (%write-plan-entry-path entry))
         (byte-count (if (stringp content)
                         (write-plan-string-byte-count content path)
                         (length content)))
         (next-total (+ total byte-count)))
    (check-limit limits :max-write-plan-bytes next-total)
    (let ((bytes
            (if (stringp content)
                (handler-case
                    (cl-codec-kit:string-to-octets content :encoding :utf-8)
                  (error () (write-plan-error :invalid-utf-8 path)))
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
                 (let* ((encoded (write-plan-encode-entry entry index total limits))
                        (next-total (fourth encoded)))
                   (setf total next-total)
                   (push (list entry path components (third encoded)) records))))
      (setf records (nreverse records))
      (write-plan-check-path-conflicts records)
      (let ((canonical-root (write-plan-canonical-root root)))
        (dolist (record records)
          (write-plan-inspect-entry canonical-root (second record) (third record)
                                    (%write-plan-entry-create-directories-p
                                     (first record))))
        (values canonical-root records)))))

(defun write-plan-revalidate-record (root record)
  (write-plan-inspect-entry root (second record) (third record)
                            (%write-plan-entry-create-directories-p
                             (first record))))

(defun write-plan-ensure-parent-directories (root record)
  (let* ((components (butlast (third record)))
         (current root))
    (loop for component in components
          do (setf current (concatenate 'string current
                                        (if (char= (char current
                                                        (1- (length current)))
                                                   #\/) "" "/")
                                        component))
             (unless (write-plan-lstat current)
               (write-plan-revalidate-record root record)
               (handler-case
                   (sb-posix:mkdir current #o755)
                 (sb-posix:syscall-error (condition)
                   (unless (write-plan-lstat current)
                     (error condition))))
               (write-plan-revalidate-record root record)))))

(defun write-plan-random-temporary-path (parent)
  (concatenate 'string parent
               (if (char= (char parent (1- (length parent))) #\/) "" "/")
               ".org-kit-"
               (format nil "~36R" (random most-positive-fixnum))
               ".tmp"))

(defun write-plan-open-temporary-file (parent)
  (loop repeat 100
        for path = (write-plan-random-temporary-path parent)
        do (handler-case
               (return (values path
                               (sb-posix:open path
                                              (logior sb-posix:o-creat
                                                      sb-posix:o-excl
                                                      sb-posix:o-wronly
                                                      sb-posix:o-nofollow)
                                              #o600)))
             (sb-posix:syscall-error (condition)
               (unless (= (sb-posix:syscall-errno condition) sb-posix:eexist)
                 (error condition))))
        finally (write-plan-error :temporary-name-exhausted parent)))

(defun write-plan-record-target (root record)
  (concatenate 'string root
               (if (char= (char root (1- (length root))) #\/) "" "/")
               (second record)))

(defun write-plan-record-parent (root record)
  (let* ((path (second record))
         (separator (position #\/ path :from-end t)))
    (if separator
        (concatenate 'string root (subseq path 0 separator))
        root)))

(defun write-plan-apply-record (root record)
  (write-plan-ensure-parent-directories root record)
  (write-plan-revalidate-record root record)
  (let* ((entry (first record))
         (bytes (fourth record))
         (parent (write-plan-record-parent root record))
         (target (write-plan-record-target root record)))
    (multiple-value-bind (temporary fd)
        (write-plan-open-temporary-file parent)
      (let ((stream nil)
            (stream-closed-p nil)
            (renamed-p nil))
        (unwind-protect
             (progn
               (setf stream
                     (sb-sys:make-fd-stream fd :output t
                                             :element-type '(unsigned-byte 8)
                                             :name temporary))
               (write-sequence bytes stream)
               (finish-output stream)
               (sb-posix:fchmod fd (%write-plan-entry-mode entry))
               (close stream)
               (setf stream-closed-p t)
               (write-plan-revalidate-record root record)
               (sb-posix:rename temporary target)
               (setf renamed-p t))
          (unless stream-closed-p
            (if stream
                (ignore-errors (close stream :abort t))
                (ignore-errors (sb-posix:close fd))))
          (unless renamed-p
            (ignore-errors (sb-posix:unlink temporary))))))
  (second record)))

(defun apply-write-plan (plan root &key (limits (make-limits)))
  "Validate PLAN in full, then atomically replace each file below ROOT.
All paths are relative POSIX paths. Every entry and limit is checked before
directory or file creation. Each file uses a same-directory temporary file and
rename; a multi-file plan is not transactional."
  (multiple-value-bind (canonical-root records)
      (write-plan-prepare plan root limits)
    (mapcar (lambda (record)
              (write-plan-apply-record canonical-root record))
            records)))

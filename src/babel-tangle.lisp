;;;; src/babel-tangle.lisp
(in-package #:org-kit)

(defparameter +babel-tangle-language-extensions+
  '(("emacs-lisp" . "el")
    ("elisp" . "el")
    ("bibtex" . "bib")))

(defstruct (babel-tangle-file
            (:constructor make-babel-tangle-file)
            (:conc-name babel-tangle-file-))
  path
  components
  (pieces nil)
  (mode #o644)
  (mode-supplied-p nil)
  (shebang-p nil)
  (create-directories-p nil))

(defstruct (babel-tangle-budget
            (:constructor make-babel-tangle-budget)
            (:conc-name babel-tangle-budget-))
  limits
  (characters 0)
  (octets 0))

(defun babel-tangle-signal-error (reason message context)
  (error 'org-babel-error :message message :reason reason
         :context (and context (bounded-text context))))

(defun babel-tangle-parameter (parameters name &optional default)
  (let ((position (position name parameters :key #'car :test #'string=)))
    (if position
        (babel-noweb-normalize-parameter-value (cdr (nth position parameters)))
        default)))

(defun babel-tangle-parameter-present-p (parameters name)
  (position name parameters :key #'car :test #'string=))

(defun babel-tangle-header-contains-parameter-p (text name)
  (and text
       (assoc name (babel-parse-header-arguments text) :test #'string=)))

(defun babel-tangle-explicit-parameter-p (document src-block name)
  (let* ((language (node-property src-block :language ""))
         (heading (babel-containing-heading src-block))
         (language-args (format nil "header-args:~A" language)))
    (or (babel-tangle-header-contains-parameter-p
         (babel-global-property-value document "header-args") name)
        (babel-tangle-header-contains-parameter-p
         (babel-global-property-value document language-args) name)
        (babel-tangle-header-contains-parameter-p
         (babel-effective-property heading "header-args") name)
        (babel-tangle-header-contains-parameter-p
         (babel-effective-property heading language-args) name)
        (babel-tangle-header-contains-parameter-p
         (node-property src-block :header) name)
        (babel-tangle-header-contains-parameter-p
         (node-property src-block :switches) name))))

(defun babel-tangle-language-extension (language)
  (or (cdr (assoc language +babel-tangle-language-extensions+
                  :test #'string=))
      language))

(defun babel-tangle-derived-path (document language)
  (let* ((source-name (document-source-name document))
         (source-path
           (typecase source-name
             (string (sb-ext:parse-native-namestring source-name))
             (pathname source-name)
             (t nil))))
    (unless (and source-path (pathname-name source-path))
      (babel-tangle-signal-error
       :missing-source-name
       "A source name is required for :tangle yes"
       source-name))
    (let* ((directory (pathname-directory source-path))
           (relative-directory
             (if (and directory (eq (first directory) :relative))
                 directory
                 '(:relative)))
           (target
             (make-pathname :directory relative-directory
                            :name (pathname-name source-path)
                            :type (babel-tangle-language-extension language))))
      (sb-ext:native-namestring target))))

(defun babel-tangle-source-relative-path (document)
  (let ((source-name (document-source-name document)))
    (when (or (stringp source-name) (pathnamep source-name))
      (let ((raw (if (pathnamep source-name)
                     (sb-ext:native-namestring source-name)
                     source-name)))
        (unless (and (plusp (length raw))
                     (char= (char raw 0) #\/))
          (handler-case (write-plan-normalized-path raw
                                                    (document-limits document))
            (org-write-plan-error () nil)))))))

(defun babel-tangle-target-path (document src-block parameters target-file)
  (let* ((tangle (babel-tangle-parameter parameters "TANGLE" "no"))
         (explicit-p (babel-tangle-explicit-parameter-p
                      document src-block "TANGLE"))
         (destination
           (cond ((and target-file (not explicit-p))
                  (if (pathnamep target-file)
                      (sb-ext:native-namestring target-file)
                      target-file))
                 ((null tangle) "no")
                 ((string-equal tangle "yes")
                  (babel-tangle-derived-path
                   document (node-property src-block :language "")))
                 (t tangle))))
    (unless (stringp destination)
      (error 'type-error :datum destination :expected-type 'string))
    (if (string-equal destination "no")
        (values nil nil)
        (multiple-value-bind (path components)
            (write-plan-normalized-path destination
                                        (document-limits document))
          (let ((source-path (babel-tangle-source-relative-path document)))
            (when (and source-path (string= path source-path))
              (write-plan-error :same-source-file path)))
          (values path components)))))

(defun babel-tangle-language-matches-p (language compiled-regexp)
  (or (null compiled-regexp)
      (and (stringp language)
           (cl-regex-kit:is-match-p compiled-regexp language))))

(defun babel-tangle-lines (text)
  (let ((lines nil)
        (start 0))
    (loop for end = (position #\Newline text :start start)
          do (push (subseq text start end) lines)
          while end
          do (setf start (1+ end)))
    (nreverse lines)))

(defun babel-tangle-indent-columns (line)
  (loop with column = 0
        for character across line
        while (find character '(#\Space #\Tab))
        do (setf column (if (char= character #\Tab)
                            (+ column (- 8 (mod column 8)))
                            (1+ column)))
        finally (return column)))

(defun babel-tangle-nonblank-line-p (line)
  (some (lambda (character)
          (not (find character '(#\Space #\Tab #\Return))))
        line))

(defun babel-tangle-remove-line-indentation (line columns)
  (loop with position = 0
        with column = 0
        while (and (< position (length line))
                   (< column columns)
                   (find (char line position) '(#\Space #\Tab)))
        for next-column = (if (char= (char line position) #\Tab)
                              (+ column (- 8 (mod column 8)))
                              (1+ column))
        do (setf column next-column)
           (incf position)
        finally
           (return
             (if (> column columns)
                 (concatenate 'string
                              (make-string (- column columns)
                                           :initial-element #\Space)
                              (subseq line position))
                 (subseq line position)))))

(defun babel-tangle-remove-indentation (text)
  (let* ((lines (babel-tangle-lines text))
         (indentation
           (loop for line in lines
                 when (babel-tangle-nonblank-line-p line)
                   minimize (babel-tangle-indent-columns line)))
         (indented-text
           (if (or (null indentation) (zerop indentation))
               text
               (format nil "~{~A~^~%~}"
                       (mapcar (lambda (line)
                                 (babel-tangle-remove-line-indentation
                                  line indentation))
                               lines)))))
    (string-trim '(#\Space #\Tab #\Newline #\Return)
                 indented-text)))

(defun babel-tangle-parse-symbolic-mode (text &optional (mode #o644))
  (labels ((operator-character-p (character)
             (find character "+-="))
           (apply-operation (operator groups permissions)
             (let ((mask 0)
                   (permissions-mask 0))
               (dolist (group groups)
                 (let ((shift (ecase group (#\u 6) (#\g 3) (#\o 0))))
                   (setf mask (logior mask (ash #o7 shift)))
                   (case group
                     (#\u (setf mask (logior mask #o4000)))
                     (#\g (setf mask (logior mask #o2000)))
                     (#\o (setf mask (logior mask #o1000))))
                   (loop for permission across permissions
                         do (case permission
                              (#\r
                               (setf permissions-mask
                                     (logior permissions-mask (ash #o4 shift))))
                              (#\w
                               (setf permissions-mask
                                     (logior permissions-mask (ash #o2 shift))))
                              ((#\x #\X)
                               (when (or (char= permission #\x)
                                         (logtest mode #o111))
                                 (setf permissions-mask
                                       (logior permissions-mask (ash #o1 shift)))))
                              ((#\u #\g #\o)
                               (let* ((source-shift
                                        (ecase permission
                                          (#\u 6) (#\g 3) (#\o 0)))
                                      (source-bits
                                        (ldb (byte 3 source-shift) mode)))
                                 (setf permissions-mask
                                       (logior permissions-mask
                                               (ash source-bits shift))))))))
               (setf mode
                     (case operator
                       (#\+ (logior mode permissions-mask))
                       (#\- (logand mode (lognot permissions-mask)))
                       (#\= (logior (logand mode (lognot mask))
                                    permissions-mask))))))))
    (loop for clause-start = 0 then (1+ comma)
          for comma = (position #\, text :start clause-start)
          for clause-end = (or comma (length text))
          for clause = (subseq text clause-start clause-end)
          do (let* ((operator-position
                      (position-if #'operator-character-p clause))
                    (who-text (and operator-position
                                   (subseq clause 0 operator-position))))
               (unless (and operator-position
                            (every (lambda (character)
                                     (find character "ugoa"))
                                   who-text))
                 (babel-tangle-signal-error
                  :invalid-tangle-mode "Invalid symbolic :tangle-mode" text))
               (let ((groups
                       (if (plusp (length who-text))
                           (remove-duplicates
                            (loop for character across who-text
                                  append (case character
                                           (#\a '(#\u #\g #\o))
                                           ((#\u #\g #\o) (list character))))
                            :test #'char=)
                           '(#\u #\g #\o))))
                 (loop with operation-position = operator-position
                       while operation-position
                       do (let* ((operator (char clause operation-position))
                                 (permissions-start (1+ operation-position))
                                 (next-operation-position
                                   (position-if #'operator-character-p clause
                                                :start permissions-start))
                                 (permissions
                                   (subseq clause permissions-start
                                           (or next-operation-position
                                               (length clause)))))
                            (unless (every (lambda (character)
                                             (find character "rwxXugo"))
                                           permissions)
                              (babel-tangle-signal-error
                               :invalid-tangle-mode
                               "Invalid symbolic :tangle-mode" text))
                            (apply-operation operator groups permissions)
                            (setf operation-position
                                  next-operation-position)))))
          while comma)
    mode))

(defun babel-tangle-mode (value)
  (let ((mode
         (cond
          ((and (integerp value) (<= 0 value #o777))
           value)
          ((not (stringp value))
           (if (integerp value)
               (babel-tangle-signal-error :invalid-tangle-mode
                                          "Invalid :tangle-mode"
                                          (format nil "~O" value))
             (error 'type-error :datum value
                    :expected-type '(or integer string))))
          ((and (member (length value) '(4 5))
                (char= (char value 0) #\o)
                (let ((digits (subseq value 1)))
                  (and (every (lambda (character)
                                (find character "01234567"))
                              digits)
                       (member (length digits) '(3 4)))))
           (parse-integer value :start 1 :radix 8))
          ((and (= (length value) 9)
                (loop for index from 0 below 9
                      for character = (char value index)
                      always
                      (case (mod index 3)
                        (0 (find character "r-"))
                        (1 (find character "w-"))
                        (2 (if (< index 8)
                               (find character "xs-")
                             (find character "x-"))))))
           (loop with mode = 0
                 for index from 0 below 9
                 for character = (char value index)
                 do (case character
                      (#\r
                       (incf mode (case index (0 #o400) (3 #o040) (6 #o004))))
                      (#\w
                       (incf mode (case index (1 #o200) (4 #o020) (7 #o002))))
                      (#\x
                       (incf mode (case index (2 #o100) (5 #o010) (8 #o001))))
                      (#\s
                       (setf mode
                             (logior mode
                                     (case index (2 #o4000) (5 #o2000))))))
                 finally (return mode)))
          ((and (plusp (length value))
                (every (lambda (character) (find character "0123456789"))
                       value))
           (let* ((number (parse-integer value))
                  (octal (format nil "~O" number)))
             (if (and (= (length octal) 3)
                      (every (lambda (character)
                               (find character "01234567"))
                             octal))
                 number
               (babel-tangle-signal-error :invalid-tangle-mode
                                          "Invalid :tangle-mode" value))))
          ((position-if (lambda (character) (find character "+-=")) value)
           (babel-tangle-parse-symbolic-mode value))
          (t
           (babel-tangle-signal-error :invalid-tangle-mode
                                      "Invalid :tangle-mode" value)))))
    (if (<= 0 mode #o777)
        mode
      (babel-tangle-signal-error
       :invalid-tangle-mode
       "Unsupported special permission bits in :tangle-mode"
       value))))

(defun babel-tangle-comment-kinds (parameters)
  (let* ((present-p (babel-tangle-parameter-present-p parameters "COMMENTS"))
         (raw (babel-tangle-parameter parameters "COMMENTS"))
         (mode (string-downcase (or raw (if present-p "yes" "no")))))
    (cond
      ((string= mode "no") nil)
      ((member mode '("link" "yes" "noweb") :test #'string=)
       '(:link))
      ((string= mode "org") '(:org))
      ((string= mode "both") '(:org :link))
      (t (babel-tangle-signal-error :unsupported-comments-mode
                                    "Unsupported :comments mode" mode)))))

(defun babel-tangle-render-comments (renderer block kind target-path)
  (unless renderer
    (babel-tangle-signal-error :missing-comment-renderer
                               "A comment renderer is required"
                               (format nil "~A comments" kind)))
  (multiple-value-bind (before after)
      (funcall renderer block kind target-path)
    (unless (stringp before)
      (babel-tangle-signal-error :invalid-comment-renderer-result
                                 "Comment renderer must return a string"
                                 kind))
    (when (and (eq kind :link) (not (stringp after)))
      (babel-tangle-signal-error :invalid-comment-renderer-result
                                 "Link comment renderer must return two strings"
                                 kind))
    (values (if (plusp (length before))
                (babel-tangle-ensure-newline before)
                "")
            (and after
                 (if (plusp (length after))
                     (babel-tangle-ensure-newline after)
                     "")))))

(defun babel-tangle-ensure-newline (text)
  (if (and (plusp (length text))
           (char= (char text (1- (length text))) #\Newline))
      text
      (concatenate 'string text (string #\Newline))))

(defun babel-tangle-add-piece (budget file text)
  (unless (stringp text)
    (error 'type-error :datum text :expected-type 'string))
  (let* ((limits (babel-tangle-budget-limits budget))
         (characters (+ (babel-tangle-budget-characters budget)
                        (length text)))
         (octets (write-plan-string-byte-count
                  text (babel-tangle-file-path file)
                  limits (babel-tangle-budget-octets budget))))
    (check-limit limits :max-output-length characters)
    (check-limit limits :max-output-octets
                 (+ (babel-tangle-budget-octets budget) octets))
    (setf (babel-tangle-budget-characters budget) characters)
    (incf (babel-tangle-budget-octets budget) octets)
    (push text (babel-tangle-file-pieces file))))

(defun babel-tangle-mode-for-block (parameters shebang)
  (let* ((position (position "TANGLE-MODE" parameters
                             :key #'car :test #'string=))
         (value (and position (cdr (nth position parameters)))))
    (cond
      (position (values (babel-tangle-mode
                         (babel-noweb-normalize-parameter-value value)) t))
      ((and shebang (plusp (length shebang))) (values #o755 t))
      (t (values nil nil)))))

(defun babel-tangle-variable-text
    (document block parameters result-resolver variable-renderer)
  (let ((variable-parameters
          (remove-if-not (lambda (parameter)
                           (and (string= (car parameter) "VAR") (cdr parameter)))
                         parameters)))
    (when variable-parameters
      (unless variable-renderer
        (babel-tangle-signal-error :missing-variable-renderer
                                   "A variable renderer is required"
                                   (node-property block :language)))
      (let* ((variables (resolve-babel-variables
                         document block :result-resolver result-resolver))
             (text (funcall variable-renderer block variables)))
        (unless (stringp text)
          (babel-tangle-signal-error :invalid-variable-renderer-result
                                     "Variable renderer must return a string"
                                     (node-property block :language)))
        (babel-noweb-normalize-body text)))))

(defun babel-tangle-block-body (document block result-resolver)
  (let ((body (expand-babel-noweb document block :context :tangle
                                  :result-resolver result-resolver)))
    (check-limit (document-limits document)
                 :max-babel-expanded-characters (length body))
    (babel-tangle-remove-indentation body)))

(defun babel-tangle-file-content (file)
  (with-output-to-string (output)
    (dolist (piece (nreverse (babel-tangle-file-pieces file)))
      (write-string piece output))))

(defun babel-tangle-plan-entries (files)
  (mapcar
   (lambda (file)
     (let ((content (babel-tangle-file-content file)))
       (if (babel-tangle-file-mode-supplied-p file)
           (make-write-plan-entry
            (babel-tangle-file-path file) content
            :mode (babel-tangle-file-mode file)
            :create-directories-p
            (babel-tangle-file-create-directories-p file))
           (make-write-plan-entry
            (babel-tangle-file-path file) content
            :create-directories-p
            (babel-tangle-file-create-directories-p file)))))
   files))

(defun babel-tangle (document
                     &key target-file language-regexp result-resolver
                       comment-renderer variable-renderer)
  "Return a write plan for the selected source blocks in DOCUMENT.

TARGET-FILE supplies the default :TANGLE path; an effective :TANGLE header,
including :TANGLE no, takes precedence. LANGUAGE-REGEXP filters language names.
RESULT-RESOLVER supplies noweb block results. COMMENT-RENDERER receives a block,
:LINK or :ORG, and target path; :LINK returns before and after strings as two
values, while :ORG returns the before string. VARIABLE-RENDERER receives a block
and its resolved variable alist and returns text to prepend to its body.
No file is written. Returned paths are relative to a root chosen by the caller.
"
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (document-root document)
    (babel-tangle-signal-error :unparsed-document
                               "Tangling requires a parsed document" nil))
  (unless (or (null target-file) (stringp target-file) (pathnamep target-file))
    (error 'type-error :datum target-file :expected-type '(or null string pathname)))
  (unless (or (null language-regexp) (stringp language-regexp))
    (error 'type-error :datum language-regexp :expected-type '(or null string)))
  (unless (or (null result-resolver) (functionp result-resolver))
    (error 'type-error :datum result-resolver :expected-type '(or null function)))
  (unless (or (null comment-renderer) (functionp comment-renderer))
    (error 'type-error :datum comment-renderer :expected-type '(or null function)))
  (unless (or (null variable-renderer) (functionp variable-renderer))
    (error 'type-error :datum variable-renderer :expected-type '(or null function)))
  (let* ((compiled-language-regexp
           (when language-regexp
             (let ((limits (document-limits document)))
               (check-limit limits :max-search-regex-length
                            (length language-regexp))
               (handler-case
                   (cl-regex-kit:compile-regex
                    language-regexp
                    :size-limit
                    (max 1 (limit-value limits :max-search-regex-length))
                    :nest-limit (limit-value limits :max-element-depth))
                 (cl-regex-kit:regex-syntax-error ()
                   (babel-tangle-signal-error
                    :invalid-language-regexp
                    "Invalid Babel language regular expression"
                    language-regexp))))))
         (files nil)
         (file-index (make-hash-table :test #'equal))
         (budget (make-babel-tangle-budget :limits (document-limits document)))
         (limits (document-limits document)))
    (map-nodes
     (lambda (block)
       (when (eq (node-type block) :src-block)
         (let ((language (node-property block :language "")))
           (when (babel-tangle-language-matches-p
                  language compiled-language-regexp)
             (let* ((parameters (resolve-babel-parameters document block))
                    (target-info
                      (multiple-value-list
                       (babel-tangle-target-path
                        document block parameters target-file)))
                    (path (first target-info))
                    (components (second target-info)))
               (when path
                 (let* ((file (or (gethash path file-index)
                                  (let ((new-file
                                          (make-babel-tangle-file
                                           :path path :components components)))
                                    (check-limit
                                     limits :max-write-plan-entries
                                     (1+ (length files)))
                                    (setf (gethash path file-index) new-file
                                          files (append files (list new-file)))
                                    new-file)))
                        (shebang (babel-tangle-parameter parameters "SHEBANG"))
                        (padline (babel-tangle-parameter parameters "PADLINE"))
                        (mkdirp-present-p
                          (babel-tangle-parameter-present-p parameters "MKDIRP"))
                        (mkdirp (babel-tangle-parameter parameters "MKDIRP")))
                   (when (and (babel-tangle-file-pieces file)
                              (not (and padline (string-equal padline "no"))))
                     (babel-tangle-add-piece budget file (string #\Newline)))
                   (when (and shebang (plusp (length shebang))
                              (not (babel-tangle-file-shebang-p file)))
                     (setf (babel-tangle-file-shebang-p file) t)
                     (babel-tangle-add-piece
                      budget file (babel-tangle-ensure-newline shebang)))
                   (when (and mkdirp-present-p
                              (or (null mkdirp)
                                  (not (string-equal mkdirp "no"))))
                     (setf (babel-tangle-file-create-directories-p file) t))
                   (multiple-value-bind (mode mode-supplied-p)
                       (babel-tangle-mode-for-block parameters shebang)
                     (when mode-supplied-p
                       (unless (babel-tangle-file-mode-supplied-p file)
                         (setf (babel-tangle-file-mode file) mode
                               (babel-tangle-file-mode-supplied-p file) t))))
                   (let ((comment-kinds
                           (babel-tangle-comment-kinds parameters))
                         (link-after nil))
                     (dolist (kind comment-kinds)
                       (multiple-value-bind (before after)
                           (babel-tangle-render-comments
                            comment-renderer block kind path)
                         (babel-tangle-add-piece budget file before)
                         (when after (setf link-after after))))
                     (let ((variables
                             (babel-tangle-variable-text
                              document block parameters result-resolver
                              variable-renderer))
                           (body (babel-tangle-block-body
                                  document block result-resolver)))
                       (when (and variables (plusp (length variables)))
                         (babel-tangle-add-piece
                          budget file (babel-tangle-ensure-newline variables)))
                       (babel-tangle-add-piece
                        budget file (babel-tangle-ensure-newline body)))
                     (when link-after
                       (babel-tangle-add-piece budget file link-after))))))))))
     (document-root document))
    (let ((records
            (loop for file in files
                  collect (list nil (babel-tangle-file-path file)
                                (babel-tangle-file-components file) nil
                                (write-plan-conflict-key
                                 (babel-tangle-file-path file))
                                (mapcar #'write-plan-conflict-key
                                        (babel-tangle-file-components file))
                                nil))))
      (write-plan-check-path-conflicts records))
    (make-write-plan (babel-tangle-plan-entries files))))

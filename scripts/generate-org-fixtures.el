;;; generate-org-fixtures.el --- Emit org-element corpus fixtures -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'org-inlinetask)
(require 'cl-lib)
(require 'subr-x)

(defconst org-fixture--selected-properties
  '((headline :title :level :todo-keyword :todo-type :priority :tags :commentedp :archivedp
              :scheduled :deadline :closed)
    (inlinetask :title :level :todo-keyword :todo-type :priority :tags :commentedp :archivedp)
    (src-block :language :value :switches :parameters :name)
    (inline-src-block :language :value :parameters)
    (link :type :path :format)
    (timestamp :type :year-start :month-start :day-start :hour-start :minute-start
               :year-end :month-end :day-end :hour-end :minute-end
               :repeater-type :repeater-value :repeater-unit
               :warning-type :warning-value :warning-unit)
    (plain-list :type)
    (item :bullet :checkbox :counter :tag)
    (table :type :tblfm)
    (table-row :type)
    (table-cell :value)
    (keyword :key :value :options)
    (special-block :type :parameters :name)
    (dynamic-block :block-name :arguments)
    (center-block :type)
    (quote-block :type)
    (verse-block :type)
    (example-block :switches :value)
    (export-block :type :value)
    (comment :value)
    (comment-block :value)
    (fixed-width :value)
    (drawer :drawer-name)
    (node-property :key :value)
    (planning :closed :deadline :scheduled)
    (clock :value :status)
    (babel-call :call :inside-header :arguments :end-header)
    (inline-babel-call :call :inside-header :arguments :end-header)
    (diary-sexp :value)
    (latex-environment :value)
    (footnote-definition :label)
    (footnote-reference :label :type)
    (citation :style :key)
    (citation-reference :key :prefix :suffix :locator)
    (entity :name :latex :latex-math-p :html :ascii :utf-8 :use-brackets-p)
    (latex-fragment :value)
    (export-snippet :back-end :value)
    (macro :key :args :value)
    (target :value)
    (radio-target :value)
    (statistics-cookie :value :type)
    (subscript :use-brackets-p)
    (superscript :use-brackets-p)))

(defun org-fixture--element-p (value)
  (and (consp value)
       (memq (car value)
             (append org-element-all-elements org-element-all-objects))
       (let ((properties (cadr value)))
         (and (listp properties)
              (or (null properties) (keywordp (car properties)))))))

(defun org-fixture--cl-string (value)
  (with-temp-buffer
    (insert "\"")
    (mapc (lambda (character)
            (when (memq character '(?\\ ?\")) (insert ?\\))
            (insert character))
          (string-to-list value))
    (insert "\"")
    (buffer-string)))

(defun org-fixture--cl-value (value)
  (cond
   ((null value) "nil")
   ((eq value t) "t")
   ((stringp value) (org-fixture--cl-string (substring-no-properties value)))
   ((symbolp value)
    (if (keywordp value)
        (downcase (symbol-name value))
      (concat ":" (downcase (symbol-name value)))))
   ((numberp value) (number-to-string value))
   ((org-fixture--element-p value) (org-fixture--node value 0))
   ((consp value)
    (let ((items nil)
          (tail value))
      (while (consp tail)
        (push (org-fixture--cl-value (car tail)) items)
        (setq tail (cdr tail)))
      (if tail
          (format "(%s . %s)"
                  (string-join (nreverse items) " ")
                  (org-fixture--cl-value tail))
        (concat "(" (string-join (nreverse items) " ") ")"))))
   ((and (vectorp value) (not (stringp value)))
    (org-fixture--cl-value (append value nil)))
   (t (error "Unsupported Org property value of type %s" (type-of value)))))

(defun org-fixture--child (value depth)
  (cond
   ((stringp value)
    (org-fixture--cl-string (substring-no-properties value)))
   ((org-fixture--element-p value)
    (org-fixture--node value depth))
   (t (org-fixture--cl-value value))))

(defun org-fixture--position-offset (position)
  (when (markerp position) (setq position (marker-position position)))
  (if (integerp position) (max 0 (1- position)) nil))

(defun org-fixture--property-plist (element)
  (let* ((type (org-element-type element))
         (selected (cdr (assq type org-fixture--selected-properties)))
         (properties (list :begin (org-fixture--position-offset
                                   (org-element-property :begin element))
                           :end (org-fixture--position-offset
                                 (org-element-property :end element))
                           :contents-begin (org-fixture--position-offset
                                            (org-element-property :contents-begin element))
                           :contents-end (org-fixture--position-offset
                                          (org-element-property :contents-end element)))))
    (dolist (property selected)
      (setq properties
            (append properties (list property (org-element-property property element)))))
    properties))

(defun org-fixture--node (element depth)
  (let* ((indent (make-string (* 2 depth) ?\s))
         (type (concat ":" (string-replace "_" "-" (symbol-name (org-element-type element)))))
         (plist (org-fixture--property-plist element))
         (plist-text
          (string-join
           (let (parts)
             (while plist
               (push (concat (org-fixture--cl-value (pop plist)) " "
                             (org-fixture--cl-value (pop plist)))
                     parts))
             (nreverse parts))
           " "))
         (children (org-element-contents element)))
    (concat "(" type " (" plist-text ")"
            (mapconcat (lambda (child)
                        (concat "\n" (make-string (* 2 (1+ depth)) ?\s)
                                (org-fixture--child child (1+ depth))))
                      children
                      "")
            (if children (concat "\n" indent ")") ")"))))

(defun org-fixture--header ()
  (format (concat
           "; Generated from org-element-parse-buffer with Emacs %s and Org %s.\n"
           "; Offsets are zero-based decoded character positions; :end is exclusive.\n"
           "; A leading U+FEFF is removed before parsing and excluded from offsets.\n"
           "; CR and LF each count as one character, so a CRLF pair counts as two.\n"
           "; org-inlinetask is loaded explicitly with its default minimum level 15.\n"
           "; Plain text is an unpropertized string.\n"
           "; Excluded: :parent (cycles/buffer references), :standard-properties (internal markers/buffer),\n"
           "; :pre-blank and :post-blank (layout counts), :path except on links (file context),\n"
           "; :CATEGORY and unselected properties such as :raw-link/:raw-value (source spellings);\n"
           "; text properties are stripped.\n")
          emacs-version (org-version)))

(defun org-fixture--read-buffer (file)
  (let ((coding-system-for-read 'utf-8-unix)
        (auto-coding-functions nil)
        (enable-local-variables nil)
        (inhibit-local-variables t))
    (insert-file-contents file))
  (when (and (> (point-max) (point-min))
             (eq (char-after (point-min)) #xfeff))
    (delete-region (point-min) (1+ (point-min))))
  (org-mode)
  (setq-local org-inlinetask-min-level 15))

(defun org-fixture--expected-files (directory)
  (unless (file-directory-p directory)
    (error "Expected directory does not exist: %s" directory))
  (let ((files (sort (directory-files-recursively directory "\\.sexp\\'")
                     #'string<)))
    (unless files (error "No .sexp files found under %s" directory))
    files))

(defun org-fixture--read-one-tree (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (goto-char (point-min))
    (let ((tree (read (current-buffer))))
      (condition-case nil
          (progn
            (read (current-buffer))
            (error "Expected exactly one S-expression in %s" file))
        (end-of-file nil))
      tree)))

(defun org-fixture--type-key (type)
  (intern (concat ":" (string-replace "_" "-" (symbol-name type)))))

(defun org-fixture--fixture-node-p (value)
  (and (consp value)
       (keywordp (car value))
       (or (eq (car value) :org-data)
           (memq (car value)
                 (mapcar #'org-fixture--type-key
                         (append org-element-all-elements org-element-all-objects))))
       (let ((properties (cadr value)))
         (and (listp properties) (eq (car-safe properties) :begin)))))

(defun org-fixture--link-form-name (value)
  (cond
   ((stringp value) (downcase value))
   ((keywordp value) (downcase (substring (symbol-name value) 1)))
   ((symbolp value) (downcase (symbol-name value)))
   (t (format "%s" value))))

(defun org-fixture--count-tree (tree counts link-types link-formats file)
  (cond
   ((stringp tree)
    (puthash :plain-text (1+ (gethash :plain-text counts 0)) counts))
   ((org-fixture--fixture-node-p tree)
    (unless (eq (car tree) :org-data)
      (puthash (car tree) (1+ (gethash (car tree) counts 0)) counts)
      (when (eq (car tree) :link)
        (let ((type (org-fixture--link-form-name (plist-get (cadr tree) :type)))
              (format (org-fixture--link-form-name (plist-get (cadr tree) :format))))
          (puthash type (1+ (gethash type link-types 0)) link-types)
          (puthash format (1+ (gethash format link-formats 0)) link-formats))))
    (dolist (child (cddr tree))
      (org-fixture--count-tree child counts link-types link-formats file)))
   (t nil)))

(defun org-fixture-check-types (corpus-directory expected-directory)
  "Count every Org element/object type in EXPECTED-DIRECTORY."
  (unless (file-directory-p corpus-directory)
    (error "Corpus directory does not exist: %s" corpus-directory))
  (let* ((corpus-files (sort (directory-files-recursively corpus-directory "\\.org\\'")
                             #'string<))
         (expected-files (org-fixture--expected-files expected-directory))
         (counts (make-hash-table :test #'eq))
         (link-types (make-hash-table :test #'equal))
         (link-formats (make-hash-table :test #'equal))
         (types (append org-element-all-elements org-element-all-objects '(plain-text)))
         (missing nil))
    (unless corpus-files (error "No .org files found under %s" corpus-directory))
    (dolist (source corpus-files)
      (let* ((relative (concat (file-relative-name source corpus-directory) ".sexp"))
             (expected (expand-file-name relative expected-directory)))
        (unless (file-exists-p expected)
          (error "Missing generated fixture: %s" relative))))
    (unless (= (length corpus-files) (length expected-files))
      (error "Expected %d fixtures for %d corpus files"
             (length expected-files) (length corpus-files)))
    (dolist (file expected-files)
      (org-fixture--count-tree (org-fixture--read-one-tree file)
                               counts link-types link-formats file))
    (princ "type\tcount\n")
    (dolist (type types)
      (let* ((key (if (eq type 'plain-text) :plain-text (org-fixture--type-key type)))
             (count (gethash key counts 0)))
        (princ (format ":%s\t%d\n" (string-remove-prefix ":" (symbol-name key)) count))
        (unless (> count 0) (push key missing))))
    (dolist (entry (list (cons "link-type" link-types)
                         (cons "link-format" link-formats)))
      (let (keys)
        (maphash (lambda (key _count) (push key keys)) (cdr entry))
        (dolist (key (sort keys #'string<))
          (princ (format "%s:%s\t%d\n"
                         (car entry) key (gethash key (cdr entry)))))))
    (when missing
      (error "Org types absent from expected fixtures: %s"
             (string-join (mapcar #'symbol-name (nreverse missing)) ", ")))))

(defun org-fixture--render-file (file)
  (with-temp-buffer
    (org-fixture--read-buffer file)
    (let ((tree (org-element-parse-buffer)))
      (concat (org-fixture--header)
              (org-fixture--node tree 0)
              "\n"))))

(defun org-fixture--check-output (text)
  (when (string-match-p "#<buffer " text)
    (error "Generated output unexpectedly contains an Emacs buffer reference"))
  (when (or (string-match-p ":type \"file\" :path \"/" text)
            (string-match-p ":type \"file\" :path \"~/" text))
    (error "Generated output unexpectedly contains an absolute link path"))
  (let ((home (expand-file-name "~")))
    (when (and (not (equal home "/")) (string-match-p (regexp-quote home) text))
      (error "Generated output unexpectedly contains the current home directory")))
  text)

(defun org-fixture-generate (corpus-directory expected-directory)
  "Generate one .sexp file per .org file under CORPUS-DIRECTORY."
  (unless (file-directory-p corpus-directory)
    (error "Corpus directory does not exist: %s" corpus-directory))
  (let ((files (sort (directory-files-recursively corpus-directory "\\.org\\'")
                     #'string<)))
    (unless files (error "No .org files found under %s" corpus-directory))
    (dolist (file files)
      (let* ((relative (file-relative-name file corpus-directory))
             (output-relative (concat relative ".sexp"))
             (output (expand-file-name output-relative expected-directory))
             (content (org-fixture--check-output (org-fixture--render-file file))))
        (make-directory (file-name-directory output) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region content nil output nil 'silent))
        (princ (format "Wrote %s\n" output-relative))))
    (princ (format "Generated %d fixture files.\n" (length files)))
  (princ (format "Emacs %s; Org %s\n" emacs-version (org-version)))))

(defun org-fixture--main ()
  (let ((arguments (if (equal (car command-line-args-left) "--")
                       (cdr command-line-args-left)
                     command-line-args-left)))
    (setq command-line-args-left nil)
    (cond
     ((and (= (length arguments) 3) (equal (car arguments) "--check-types"))
      (org-fixture-check-types (expand-file-name (nth 1 arguments))
                               (expand-file-name (nth 2 arguments))))
     ((= (length arguments) 2)
      (org-fixture-generate (expand-file-name (nth 0 arguments))
                            (expand-file-name (nth 1 arguments))))
     (t
      (error (concat
              "Usage: emacs --batch -Q --script scripts/generate-org-fixtures.el CORPUS-DIR EXPECTED-DIR\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --check-types CORPUS-DIR EXPECTED-DIR"))))))

(org-fixture--main)

;;; generate-org-fixtures.el ends here

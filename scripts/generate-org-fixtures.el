;;; generate-org-fixtures.el --- Emit org-element corpus fixtures -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'org-clock)
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

(defvar org-fixture--include-effective-tags nil)

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
    (when (and org-fixture--include-effective-tags (eq type 'headline))
      (setq properties
            (append properties
                    (list :effective-tags
                          (save-excursion
                            (goto-char (org-element-property :begin element))
                            (org-get-tags))))))
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
  (let ((header
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
                 emacs-version (org-version))))
    (if org-fixture--include-effective-tags
        (concat header "; :effective-tags is captured from org-get-tags at each headline.\n")
        header)))

(defun org-fixture--read-buffer (file &optional inlinetask-min-level)
  (let ((coding-system-for-read 'utf-8-unix)
        (auto-coding-functions nil)
        (enable-local-variables nil)
        (inhibit-local-variables t))
    (insert-file-contents file)
    (when (and (> (point-max) (point-min))
               (eq (char-after (point-min)) #xfeff))
      (delete-region (point-min) (1+ (point-min))))
    (let ((org-inlinetask-min-level (or inlinetask-min-level 15)))
      (org-mode))
    (setq-local org-inlinetask-min-level (or inlinetask-min-level 15))))

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

(defun org-fixture--g1-clock-parse-time (text)
  (unless (and (stringp text)
               (string-match
                "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\'"
                text))
    (error "Invalid G1 clock fixture datetime: %S" text))
  (let ((year (string-to-number (match-string 1 text)))
        (month (string-to-number (match-string 2 text)))
        (day (string-to-number (match-string 3 text)))
        (hour (string-to-number (match-string 4 text)))
        (minute (string-to-number (match-string 5 text)))
        (second (string-to-number (match-string 6 text))))
    (unless (and (<= 1 month 12)
                 (<= 1 day 31)
                 (<= 0 hour 23)
                 (<= 0 minute 59)
                 (<= 0 second 59))
      (error "Out-of-range G1 clock fixture datetime: %S" text))
    (encode-time (list second minute hour day month year))))

(defun org-fixture--g1-clock-read-cases (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix)
          (read-eval nil))
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((data (read (current-buffer))))
        (skip-chars-forward " \t\n\r")
        (unless (and (eobp)
                     (listp data)
                     (eq (car-safe data) :cases)
                     (= (length data) 2)
                     (proper-list-p (cadr data))
                     (cadr data))
          (error "Expected one non-empty (:cases ...) declaration in %s" file))
        (cadr data)))))

(defun org-fixture--g1-clock-validate-cases (cases)
  (let ((ids (make-hash-table :test #'equal))
        (known-keys '(:id :action :source :heading-path :from :to
                      :reference-time :zone :dst-policy :include-running-p
                      :include-unscoped-p :active-clock-start :as-of))
        (required-keys '(:id :action :source :heading-path :from :to
                         :reference-time :zone :dst-policy :include-running-p
                         :include-unscoped-p)))
    (dolist (case cases)
      (unless (and (proper-list-p case) (eq (car-safe case) :id))
        (error "Malformed G1 clock case: %S" case))
      (let ((plist case)
            (seen nil))
        (while plist
          (unless (and (consp plist)
                       (consp (cdr plist))
                       (memq (car plist) known-keys)
                       (not (memq (car plist) seen)))
            (error "Malformed G1 clock case property list: %S" case))
          (push (car plist) seen)
          (setq plist (cddr plist)))
        (unless (cl-every (lambda (key) (memq key seen)) required-keys)
          (error "Missing required G1 clock case property: %S" case))
        (let ((id (plist-get case :id))
              (path (plist-get case :heading-path))
              (include-running (plist-get case :include-running-p))
              (has-active-start (plist-member case :active-clock-start))
              (has-as-of (plist-member case :as-of)))
          (unless (and (stringp id)
                       (not (gethash id ids))
                       (eq (plist-get case :action) :sum-clocks)
                       (equal (plist-get case :source) "g1-clock.org")
                       (and (proper-list-p path) path (cl-every #'stringp path))
                       (stringp (plist-get case :from))
                       (stringp (plist-get case :to))
                       (stringp (plist-get case :reference-time))
                       (stringp (plist-get case :zone))
                       (memq (plist-get case :dst-policy)
                             '(:compatible :earlier :later :strict))
                       (memq include-running '(nil t))
                       (memq (plist-get case :include-unscoped-p) '(nil t))
                       (eq (and has-active-start t) (and has-as-of t))
                       (or (not include-running) has-active-start))
            (error "Invalid G1 clock case: %S" case))
          (when has-active-start
            (unless (and (stringp (plist-get case :active-clock-start))
                         (stringp (plist-get case :as-of)))
              (error "Invalid running-clock fields in G1 clock case: %S" case)))
          (puthash id t ids))))
    cases))

(defun org-fixture--g1-clock-heading-position (path)
  (let ((current-path nil)
        found)
    (save-excursion
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward "^\\(\\*+\\)[ \t]+.*$" nil t))
        (let* ((position (match-beginning 0))
               (level (length (match-string 1)))
               (title (save-excursion
                        (goto-char position)
                        (org-get-heading t t t t))))
          (setq current-path
                (append (cl-subseq current-path 0
                                   (min (1- level) (length current-path)))
                        (list title)))
          (when (equal current-path path)
            (setq found position)))))
    (or found (error "G1 clock heading path not found: %S" path))))

(defun org-fixture--g1-clock-open-position (heading-position start-time)
  (let ((line (format "CLOCK: [%s]"
                      (format-time-string "%Y-%m-%d %a %H:%M" start-time)))
        (subtree-end (save-excursion
                       (goto-char heading-position)
                       (org-end-of-subtree t t)
                       (point))))
    (save-excursion
      (goto-char heading-position)
      (forward-line 1)
      (unless (re-search-forward
               (concat "^[ \t]*" (regexp-quote line) "[ \t]*$")
               subtree-end t)
        (error "G1 active clock line missing under heading at %d" heading-position))
      (line-beginning-position))))

(defun org-fixture--g1-clock-case-minutes (case corpus-directory)
  (let* ((plist case)
         (zone (plist-get plist :zone))
         (source (expand-file-name (plist-get plist :source) corpus-directory))
         (process-environment (copy-sequence process-environment))
         (system-time-locale "C")
         (org-clock-report-include-clocking-task
          (plist-get plist :include-running-p))
         (org-clock-marker (make-marker))
         (org-clock-hd-marker (make-marker))
         org-clock-start-time)
    (unless (file-exists-p source)
      (error "G1 clock corpus file does not exist: %s" (plist-get plist :source)))
    (setenv "TZ" zone)
    (let* ((from (float-time
                  (org-fixture--g1-clock-parse-time (plist-get plist :from))))
           (to (float-time
                (org-fixture--g1-clock-parse-time (plist-get plist :to))))
           (reference-time
            (org-fixture--g1-clock-parse-time (plist-get plist :reference-time)))
           (as-of (if (plist-member plist :as-of)
                      (org-fixture--g1-clock-parse-time (plist-get plist :as-of))
                    reference-time))
           (active-start (and (plist-member plist :active-clock-start)
                              (org-fixture--g1-clock-parse-time
                               (plist-get plist :active-clock-start)))))
      (when (> from to)
        (error "G1 clock range ends before it starts: %S" (plist-get plist :id)))
      (unless (= (float-time reference-time) (float-time as-of))
        (error "G1 active clock as-of differs from reference-time: %S"
               (plist-get plist :id)))
      (setq org-clock-start-time active-start)
      (with-temp-buffer
        (org-fixture--read-buffer source)
        (let* ((heading-position
                (org-fixture--g1-clock-heading-position
                 (plist-get plist :heading-path)))
               (active-position
                (and active-start
                     (org-fixture--g1-clock-open-position heading-position active-start))))
          (when active-position
            (set-marker org-clock-marker active-position (current-buffer))
            (set-marker org-clock-hd-marker heading-position (current-buffer)))
          (goto-char heading-position)
          (org-narrow-to-subtree)
          (cl-letf (((symbol-function 'current-time) (lambda () as-of))
                    ((symbol-function 'current-time-string)
                     (lambda (&optional time)
                       (format-time-string "%a %b %e %T %Y" (or time as-of))))
                    ;; Emacs 31's time-since can consult the system clock directly.
                    ((symbol-function 'time-since)
                     (lambda (time) (time-subtract as-of time))))
            (org-clock-sum from to))
          (unless (integerp org-clock-file-total-minutes)
            (error "Org did not return an integer clock total for %S"
                   (plist-get plist :id)))
          org-clock-file-total-minutes)))))

(defun org-fixture-generate-g1-clock
    (corpus-directory cases-file expected-file)
  (unless (file-directory-p corpus-directory)
    (error "Corpus directory does not exist: %s" corpus-directory))
  (let* ((cases (org-fixture--g1-clock-validate-cases
                 (org-fixture--g1-clock-read-cases cases-file)))
         (results
          (mapcar (lambda (case)
                    (list :id (plist-get case :id)
                          :org-clock-minutes
                          (org-fixture--g1-clock-case-minutes case corpus-directory)))
                  cases))
         (content
          (org-fixture--check-output
           (concat
            (format (concat
                     "; Generated from org-clock-sum with Emacs %s and Org %s.\n"
                     "; Source encoding: UTF-8. Case zones are bound as TZ per oracle call.\n"
                     "; Running-clock as-of and current-time are fixed by each case.\n"
                     "; DST gap/overlap values record Org's local-time behavior; :dst-policy is case input.\n")
                    emacs-version (org-version))
            "("
            (mapconcat #'org-fixture--cl-value results "\n ")
            ")\n"))))
    (make-directory (file-name-directory expected-file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region content nil expected-file nil 'silent))
    (princ (format "Wrote %s\n" (file-name-nondirectory expected-file)))
    (princ (format "Generated %d G1 clock cases.\n" (length cases)))
    (princ (format "Emacs %s; Org %s\n" emacs-version (org-version)))))

(defun org-fixture--g2-clocktable-read-cases (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix)
          (read-eval nil))
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((data (read (current-buffer))))
        (skip-chars-forward " \t\n\r")
        (unless (and (eobp)
                     (listp data)
                     (eq (car-safe data) :cases)
                     (= (length data) 2)
                     (proper-list-p (cadr data))
                     (cadr data))
          (error "Expected one non-empty (:cases ...) declaration in %s" file))
        (cadr data)))))

(defun org-fixture--g2-clocktable-validate-cases (cases)
  (let ((ids (make-hash-table :test #'equal))
        (known-keys '(:id :action :source :heading-path :from :to :zone
                      :match :max-depth :inlinetask-min-level))
        (required-keys '(:id :action :source :heading-path :from :to :zone)))
    (dolist (case cases)
      (unless (and (proper-list-p case) (eq (car-safe case) :id))
        (error "Malformed G2 clocktable case: %S" case))
      (let ((plist case)
            (seen nil))
        (while plist
          (unless (and (consp plist)
                       (consp (cdr plist))
                       (memq (car plist) known-keys)
                       (not (memq (car plist) seen)))
            (error "Malformed G2 clocktable case property list: %S" case))
          (push (car plist) seen)
          (setq plist (cddr plist)))
        (unless (cl-every (lambda (key) (memq key seen)) required-keys)
          (error "Missing required G2 clocktable case property: %S" case))
        (let ((id (plist-get case :id))
              (path (plist-get case :heading-path))
              (depth (plist-get case :max-depth))
              (inlinetask-min-level (plist-get case :inlinetask-min-level))
              (match (plist-get case :match)))
          (unless (and (stringp id)
                       (not (gethash id ids))
                       (eq (plist-get case :action) :clocktable-data)
                       (member (plist-get case :source)
                               '("g2-clocktable.org" "g2-clocktable-depth.org"))
                       (proper-list-p path)
                       path
                       (cl-every #'stringp path)
                       (stringp (plist-get case :from))
                       (stringp (plist-get case :to))
                       (stringp (plist-get case :zone))
                       (or (null match) (stringp match))
                       (or (not (plist-member case :max-depth))
                           (and (integerp depth) (>= depth 0)))
                       (or (not (plist-member case :inlinetask-min-level))
                           (and (integerp inlinetask-min-level)
                                (> inlinetask-min-level 0))))
            (error "Invalid G2 clocktable case: %S" case))
          (puthash id t ids))))
    cases))

(defun org-fixture--g2-clocktable-heading-position (path)
  (let ((current-path nil)
        found)
    (save-excursion
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward "^\\(\\*+\\)[ \t]+.*$" nil t))
        (let* ((position (match-beginning 0))
               (level (length (match-string 1)))
               (title (save-excursion
                        (goto-char position)
                        (org-get-heading t t t t))))
          (setq current-path
                (append (cl-subseq current-path 0
                                   (min (1- level) (length current-path)))
                        (list title)))
          (when (equal current-path path)
            (setq found position)))))
    (or found (error "G2 clocktable heading path not found: %S" path))))

(defun org-fixture--g2-clocktable-org-time (text)
  (cond
   ((string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" text)
    text)
   ((string-match-p
     "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:00\\'"
     text)
    (format-time-string "%Y-%m-%d %H:%M"
                        (org-fixture--g1-clock-parse-time text)))
   (t (error "Invalid G2 clocktable range endpoint: %S" text))))

(defun org-fixture--g2-clocktable-case-result (case corpus-directory)
  (let* ((source-name (plist-get case :source))
         (source (expand-file-name source-name corpus-directory))
         (zone (plist-get case :zone))
         (process-environment (copy-sequence process-environment))
         (system-time-locale "C")
         (org-clock-report-include-clocking-task nil)
         (inlinetask-min-level (or (plist-get case :inlinetask-min-level) 15))
         (from (org-fixture--g2-clocktable-org-time (plist-get case :from)))
         (to (org-fixture--g2-clocktable-org-time (plist-get case :to)))
         (max-depth (if (plist-member case :max-depth)
                        (plist-get case :max-depth)
                      30)))
    (unless (file-exists-p source)
      (error "G2 clocktable corpus file does not exist: %s" source-name))
    (setenv "TZ" zone)
    (with-temp-buffer
      (org-fixture--read-buffer source inlinetask-min-level)
      (let ((heading-position
             (org-fixture--g2-clocktable-heading-position
              (plist-get case :heading-path))))
        (goto-char heading-position)
        (org-narrow-to-subtree)
        (goto-char (point-max))
        (let* ((table
                (org-clock-get-table-data
                 source-name
                 (list :maxlevel max-depth
                       :match (plist-get case :match)
                       :tags t
                       :tstart from
                       :tend to)))
               (rows
                (mapcar (lambda (entry)
                          (list :level (nth 0 entry)
                                :title (substring-no-properties (nth 1 entry))
                                :tags (mapcar #'substring-no-properties (nth 2 entry))
                                :minutes (nth 4 entry)))
                        (nth 2 table))))
          (list :id (plist-get case :id)
                :org-clock-total-minutes (nth 1 table)
                :org-clock-rows rows))))))

(defun org-fixture-generate-g2-clocktable
    (corpus-directory cases-file expected-file)
  (unless (file-directory-p corpus-directory)
    (error "Corpus directory does not exist: %s" corpus-directory))
  (let* ((cases (org-fixture--g2-clocktable-validate-cases
                 (org-fixture--g2-clocktable-read-cases cases-file)))
         (results (mapcar (lambda (case)
                            (org-fixture--g2-clocktable-case-result
                             case corpus-directory))
                          cases))
         (content
          (org-fixture--check-output
           (concat
            (format (concat
                     "; Generated from org-clock-get-table-data with Emacs %s and Org %s.\n"
                     "; Each case narrows to its selected heading subtree.\n"
                     "; Time zones are bound per case; open clocks are excluded.\n"
                     "; Rows use reduced levels, effective tags, and matched subtree minutes.\n"
                     "; Inline-task minimum level is case-bound (default 15).\n")
                    emacs-version (org-version))
            "(\n"
            (mapconcat #'org-fixture--cl-value results "\n ")
            ")\n"))))
    (make-directory (file-name-directory expected-file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region content nil expected-file nil 'silent))
    (princ (format "Wrote %s\n" (file-name-nondirectory expected-file)))
    (princ (format "Generated %d G2 clocktable cases.\n" (length cases)))
    (princ (format "Emacs %s; Org %s\n" emacs-version (org-version)))))

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

(defconst org-fixture--table-calc-cases
  '(("@1$1=2/3*4" ((1 . 1)))
    ("@1$1=2+3/4" ((1 . 1)))
    ("@1$1=2+3*4/5" ((1 . 1)))
    ("@1$1=12/3/2" ((1 . 1)))
    ("@1$1=-2*3" ((1 . 1)))
    ("@1$1=2*-3" ((1 . 1)))
    ("@1$1=2--3" ((1 . 1)))
    ("@1$1=(1+2)*3" ((1 . 1)))
    ("@1$1=1e-3" ((1 . 1)))
    ("@2$3=$1+$2" ((2 . 3)))
    ("@2$3=@+1$-2+@+1$-1" ((2 . 3)))
    ("@2$3=@>$1" ((2 . 3)))
    ("@2$3=vsum(@1$1..@3$1)" ((2 . 3)))
    ("@2$3=vmean(@1$1..@3$1)" ((2 . 3)))
    ("@2$3=vmin(@1$1..@3$1)" ((2 . 3)))
    ("@2$3=vmax(@1$1..@3$1)" ((2 . 3)))
    ("@2$3=vcount(@1$1..@3$1)" ((2 . 3)))
    ("@1$3=vsum($1..$2)" ((1 . 3)))
    ("@2$3=vsum(@3$1..@1$1)" ((2 . 3)))
    ("@2$3=@>" ((2 . 3)))
    ("@2$3=$x-1" ((2 . 3)))
    ("$3=$1+$2::@2$3=$1*10" ((1 . 3) (2 . 3) (3 . 3))))
  "Exact formula inputs permitted in the table-calc oracle corpus.")

(defun org-fixture--table-calc-directive-count (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (let ((case-fold-search t)
          (count 0))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+TBLFM:" nil t)
        (setq count (1+ count)))
      count)))

(defun org-fixture--table-calc-formula (table)
  (let ((formulas (org-element-property :tblfm table)))
    (unless (= (length formulas) 1)
      (error "Oracle tables must have exactly one TBLFM directive"))
    (car formulas)))

(defun org-fixture--table-calc-values (table targets)
  (let ((rows (org-element-map table 'table-row #'identity)))
    (mapcar (lambda (target)
              (let* ((row (nth (1- (car target)) rows))
                     (cells (and row
                                 (org-element-map row 'table-cell #'identity)))
                     (cell (and cells (nth (1- (cdr target)) cells)))
                     (value (and cell
                                 (mapconcat
                                  (lambda (part)
                                    (if (stringp part)
                                        (substring-no-properties part)
                                      ""))
                                  (org-element-contents cell) ""))))
                (unless (stringp value)
                  (error "Oracle formula target did not produce a table cell"))
                value))
            targets)))

(defun org-fixture--render-table-calc-file (file)
  (with-temp-buffer
    (org-fixture--read-buffer file)
    (let* ((tables (org-element-map (org-element-parse-buffer) 'table #'identity))
           (directive-count (org-fixture--table-calc-directive-count file))
           (formulas (mapcar #'org-fixture--table-calc-formula tables)))
      (unless (and tables (= directive-count (length tables)))
        (error "Every oracle TBLFM directive must belong to one table"))
      (dolist (formula formulas)
        (unless (assoc formula org-fixture--table-calc-cases)
          (error "Formula is outside the table-calc oracle allowlist")))
      (org-table-recalculate-buffer-tables)
      (let* ((calculated-tables
               (org-element-map (org-element-parse-buffer) 'table #'identity))
             (entries
               (mapcar (lambda (table)
                         (let* ((formula (org-fixture--table-calc-formula table))
                                (targets (cadr (assoc formula
                                                      org-fixture--table-calc-cases)))
                                (values (org-fixture--table-calc-values table targets)))
                           (cons formula (string-join values ","))))
                       calculated-tables)))
        (unless (= (length entries) (length formulas))
          (error "Oracle recalculation changed the table count"))
        (concat (format (concat
                         "; Generated by org-table-recalculate-buffer-tables with Emacs %s and Org %s.\n"
                         "; Inputs are checked against an exact literal allowlist before recalculation.\n"
                         "; Each following row is a formula, a tab, and the resulting target cell values.\n")
                        emacs-version (org-version))
                (mapconcat (lambda (entry)
                             (format "%s\t%s\n" (car entry) (cdr entry)))
                           entries ""))))))

(defun org-fixture-generate-table-calc (corpus-directory expected-directory)
  "Generate safe table formula oracle manifests from CORPUS-DIRECTORY."
  (unless (file-directory-p corpus-directory)
    (error "Table-calc corpus directory does not exist: %s" corpus-directory))
  (let ((files (sort (directory-files-recursively corpus-directory "\\.org\\'")
                     #'string<)))
    (unless files (error "No table-calc oracle files found"))
    (dolist (file files)
      (let* ((relative (concat (file-name-sans-extension
                                (file-relative-name file corpus-directory))
                               ".tsv"))
             (output (expand-file-name relative expected-directory))
             (content (org-fixture--render-table-calc-file file)))
        (make-directory (file-name-directory output) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region content nil output nil 'silent))
        (princ (format "Wrote table-calc oracle %s\n" relative))))
    (princ (format "Generated %d table-calc oracle manifests.\n" (length files)))
    (princ (format "Emacs %s; Org %s\n" emacs-version (org-version)))))

;;; T2 table alignment fixtures

(defconst org-fixture--table-align-required-metadata
  '("GNU Emacs 31.1; Org 9.8.10"
    "Generator mode: org-table-align"
    "Backend: none"
    "Encoding: UTF-8"
    "Reference time: 2000-01-01T00:00:00Z (fixed; unused)"
    "Zone/DST policy: UTC; DST not applicable"
    "Unicode width data: Unicode 17.0.0"))

(defun org-fixture--table-align-render-file (file)
  (with-temp-buffer
    (org-fixture--read-buffer file)
    (let ((source (buffer-string)))
      (dolist (metadata org-fixture--table-align-required-metadata)
        (unless (string-match-p (regexp-quote metadata) source)
          (error "Missing T2 fixture metadata %S in %s" metadata file))))
    (goto-char (point-min))
    (unless (re-search-forward "^[ \t]*|" nil t)
      (error "No Org pipe table found in %s" file))
    (goto-char (match-beginning 0))
    (org-table-align)
    (buffer-string)))

(defun org-fixture-generate-table-align (corpus-directory expected-directory)
  "Generate org-table-align results for T2 CORPUS-DIRECTORY fixtures."
  (unless (and (equal emacs-version "31.1")
               (equal (org-version) "9.8.10"))
    (error "T2 oracle requires Emacs 31.1 and Org 9.8.10; found %s and %s"
           emacs-version (org-version)))
  (unless (file-directory-p corpus-directory)
    (error "Corpus directory does not exist: %s" corpus-directory))
  (let ((files (sort (directory-files-recursively corpus-directory "\\.org\\'")
                     #'string<)))
    (unless files (error "No .org files found under %s" corpus-directory))
    (dolist (file files)
      (let* ((relative (file-relative-name file corpus-directory))
             (output (expand-file-name relative expected-directory))
             (content (org-fixture--check-output
                       (org-fixture--table-align-render-file file))))
        (make-directory (file-name-directory output) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region content nil output nil 'silent))
        (princ (format "Wrote %s\n" relative))))
    (princ (format "Generated %d T2 table-align fixtures.\n" (length files)))
    (princ (format "Emacs %s; Org %s\n" emacs-version (org-version)))))

(defun org-fixture--table-align-main ()
  (let ((arguments (if (equal (car command-line-args-left) "--")
                       (cdr command-line-args-left)
                     command-line-args-left)))
    (setq command-line-args-left nil)
    (if (and (= (length arguments) 3)
             (equal (car arguments) "--table-align"))
        (org-fixture-generate-table-align
         (expand-file-name (nth 1 arguments))
         (expand-file-name (nth 2 arguments)))
      (error (concat
              "Usage: emacs --batch -Q --script scripts/generate-org-fixtures.el "
              "--table-align CORPUS-DIR EXPECTED-DIR")))))

(defun org-fixture--dispatch-main ()
  (if (equal (car command-line-args-left) "--table-align")
      (org-fixture--table-align-main)
    (org-fixture--main)))

;;; End T2 table alignment fixtures

(defun org-fixture--main ()
  (let ((arguments (if (equal (car command-line-args-left) "--")
                       (cdr command-line-args-left)
                     command-line-args-left)))
    (setq command-line-args-left nil)
    (cond
     ((and (= (length arguments) 4) (equal (car arguments) "--g2-clocktable"))
      (org-fixture-generate-g2-clocktable
       (expand-file-name (nth 1 arguments))
       (expand-file-name (nth 2 arguments))
       (expand-file-name (nth 3 arguments))))
     ((and (= (length arguments) 4) (equal (car arguments) "--g1-clock"))
      (org-fixture-generate-g1-clock
       (expand-file-name (nth 1 arguments))
       (expand-file-name (nth 2 arguments))
       (expand-file-name (nth 3 arguments))))
     ((and (= (length arguments) 3) (equal (car arguments) "--check-types"))
      (org-fixture-check-types (expand-file-name (nth 1 arguments))
                               (expand-file-name (nth 2 arguments))))
     ((and (= (length arguments) 3)
           (equal (car arguments) "--include-effective-tags"))
      (let ((org-fixture--include-effective-tags t))
        (org-fixture-generate (expand-file-name (nth 1 arguments))
                              (expand-file-name (nth 2 arguments)))))
     ((and (= (length arguments) 3) (equal (car arguments) "--table-calc"))
      (org-fixture-generate-table-calc (expand-file-name (nth 1 arguments))
                                       (expand-file-name (nth 2 arguments))))
     ((= (length arguments) 2)
      (org-fixture-generate (expand-file-name (nth 0 arguments))
                            (expand-file-name (nth 1 arguments))))
     (t
      (error (concat
              "Usage: emacs --batch -Q --script scripts/generate-org-fixtures.el CORPUS-DIR EXPECTED-DIR\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --g2-clocktable CORPUS-DIR CASES-FILE EXPECTED-FILE\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --g1-clock CORPUS-DIR CASES-FILE EXPECTED-FILE\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --include-effective-tags CORPUS-DIR EXPECTED-DIR\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --table-calc CORPUS-DIR EXPECTED-DIR\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --check-types CORPUS-DIR EXPECTED-DIR"))))))

(org-fixture--dispatch-main)

;;; generate-org-fixtures.el ends here

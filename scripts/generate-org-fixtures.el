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

(defconst org-fixture--export-backends
  '((html . ("html" . html))
    (gfm . ("gfm" . gfm))
    (ascii . ("txt" . ascii))))

(defconst org-fixture--ox-gfm-version "20231215.1901")
(defconst org-fixture--ox-gfm-repository "https://github.com/larstvei/ox-gfm")
(defconst org-fixture--ox-gfm-commit "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb")
(defconst org-fixture--ox-gfm-source-sha256
  "84c841f4204447eec317453ebab514741d22d4b65459a652eb87e04b29483771")

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

(defun org-fixture--h1-clock-validate-cases (cases)
  (let ((ids (make-hash-table :test #'equal))
        (known-keys '(:id :source :heading-path :at :end-at :zone
                      :capture-document
                      :clock-into-drawer :log-into-drawer))
        (required-keys '(:id :source :heading-path :at :zone
                         :clock-into-drawer :log-into-drawer)))
    (dolist (case cases)
      (unless (and (proper-list-p case) (eq (car-safe case) :id))
        (error "Malformed H1 clock case: %S" case))
      (let ((plist case)
            (seen nil))
        (while plist
          (unless (and (consp plist)
                       (consp (cdr plist))
                       (memq (car plist) known-keys)
                       (not (memq (car plist) seen)))
            (error "Malformed H1 clock case property list: %S" case))
          (push (car plist) seen)
          (setq plist (cddr plist)))
        (unless (cl-every (lambda (key) (memq key seen)) required-keys)
          (error "Missing required H1 clock case property: %S" case))
        (let ((id (plist-get case :id))
              (path (plist-get case :heading-path))
              (end-at-present-p (plist-member case :end-at))
              (capture-document-present-p
               (plist-member case :capture-document))
              (capture-document (plist-get case :capture-document))
              (clock-drawer (plist-get case :clock-into-drawer))
              (log-drawer (plist-get case :log-into-drawer)))
          (unless (and (stringp id)
                       (not (gethash id ids))
                       (member (plist-get case :source)
                               '("h1-clock.org" "h1-clock-threshold.org"))
                       (and (proper-list-p path) path (cl-every #'stringp path))
                       (stringp (plist-get case :at))
                       (stringp (plist-get case :zone))
                       (or (null clock-drawer)
                           (eq clock-drawer t)
                           (stringp clock-drawer)
                           (and (integerp clock-drawer)
                                (>= clock-drawer 0)))
                       (or (null log-drawer)
                           (eq log-drawer t)
                           (stringp log-drawer))
                       (or (not capture-document-present-p)
                           (memq capture-document '(nil t)))
                       (or (not end-at-present-p)
                           (stringp (plist-get case :end-at))))
            (error "Invalid H1 clock case: %S" case))
          (puthash id t ids))))
    cases))

(defun org-fixture--h1-clock-line (heading-position)
  (let ((subtree-end (save-excursion
                       (goto-char heading-position)
                       (org-end-of-subtree t t)
                       (point))))
    (save-excursion
      (goto-char heading-position)
      (forward-line 1)
      (when (re-search-forward "^[ \t]*CLOCK:.*$" subtree-end t)
        (match-string-no-properties 0)))))

(defun org-fixture--h1-clock-open-line ()
  (save-excursion
    (goto-char org-clock-marker)
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun org-fixture--h1-clock-drawer-name ()
  (let* ((clock (save-excursion
                  (goto-char org-clock-marker)
                  (org-element-at-point)))
         (drawer (and (org-element-type-p clock 'clock)
                      (org-element-lineage clock '(drawer) t))))
    (and drawer (org-element-property :drawer-name drawer))))

(defun org-fixture--h1-clock-case (case corpus-directory)
  (let* ((source-name (plist-get case :source))
         (source (expand-file-name source-name corpus-directory))
         (zone (plist-get case :zone))
         (process-environment (copy-sequence process-environment))
         (system-time-locale "C")
         (org-clock-into-drawer (plist-get case :clock-into-drawer))
         (org-log-into-drawer (plist-get case :log-into-drawer))
         (org-clock-marker (make-marker))
         (org-clock-hd-marker (make-marker))
         (org-clock-clocked-in-display nil)
         (org-clock-out-remove-zero-time-clocks nil)
         (org-clock-idle-timer nil)
         (org-clock-mode-line-timer nil)
         (org-clock-auto-clockout-timer nil)
         (org-clock-current-task nil)
         org-clock-start-time
         org-clock-out-time)
    (unless (file-exists-p source)
      (error "H1 clock corpus file does not exist: %s" source-name))
    (setenv "TZ" zone)
    (let* ((start-time
            (org-fixture--g1-clock-parse-time (plist-get case :at)))
           (end-time
            (and (plist-member case :end-at)
                 (org-fixture--g1-clock-parse-time (plist-get case :end-at)))))
      (when (and end-time (> (float-time start-time) (float-time end-time)))
        (error "H1 clock end precedes start for case %S" (plist-get case :id)))
      (with-temp-buffer
        (org-fixture--read-buffer source)
        (let ((heading-position
               (org-fixture--g1-clock-heading-position
                (plist-get case :heading-path))))
          (goto-char heading-position)
          (org-clock-in nil start-time)
          (let* ((drawer-name (org-fixture--h1-clock-drawer-name))
                 (open-line (org-fixture--h1-clock-open-line))
                 (source-after-in
                  (and (plist-get case :capture-document)
                       (buffer-substring-no-properties (point-min) (point-max)))))
            (org-clock-out nil nil (or end-time start-time))
            (list :id (plist-get case :id)
                  :org-drawer-name drawer-name
                  :org-open-line open-line
                  :org-source-after-in source-after-in
                  :org-closed-line
                  (and end-time
                       (org-fixture--h1-clock-line heading-position)))))))))

(defun org-fixture-generate-h1-clock
    (corpus-directory cases-file expected-file)
  (unless (file-directory-p corpus-directory)
    (error "H1 clock corpus directory does not exist: %s" corpus-directory))
  (let* ((cases (org-fixture--h1-clock-validate-cases
                 (org-fixture--g1-clock-read-cases cases-file)))
         (results (mapcar (lambda (case)
                            (org-fixture--h1-clock-case case corpus-directory))
                          cases))
         (content
          (org-fixture--check-output
           (concat
            (format (concat
                     "; Generated from org-clock-in and org-clock-out with Emacs %s and Org %s.\n"
                     "; Case timestamps are explicit; TZ and locale are bound per oracle call.\n")
                    emacs-version (org-version))
            "("
            (mapconcat #'org-fixture--cl-value results "\n ")
            ")\n"))))
    (make-directory (file-name-directory expected-file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region content nil expected-file nil 'silent))
    (princ (format "Wrote %s\n" (file-name-nondirectory expected-file)))
    (princ (format "Generated %d H1 clock cases.\n" (length cases)))
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
    ("$3=$1+$2::@2$3=$1*10" ((1 . 3) (2 . 3) (3 . 3)))
    ("@1$2=vsum(@1$1..@3$1)" ((1 . 2)))
    ("@1$2=vmean(@1$1..@3$1)" ((1 . 2)))
    ("@1$2=vmin(@1$1..@3$1)" ((1 . 2)))
    ("@1$2=vmax(@1$1..@3$1)" ((1 . 2)))
    ("@1$2=vcount(@1$1..@3$1)" ((1 . 2)))
    ("@1$2=vcount(@1$1..@3$1);E" ((1 . 2)))
    ("@1$2=vsum(@1$1..@3$1);EN" ((1 . 2)))
    ("@1$2=vmean(@1$1..@3$1);EN%.2f" ((1 . 2)))
    ("@1$2=vmin(@1$1..@3$1);EN" ((1 . 2)))
    ("@1$2=vmax(@1$1..@3$1);EN" ((1 . 2)))
    ("@1$2=vcount(@1$1..@3$1);EN" ((1 . 2)))
    ("@1$2=vsum(@1$1..@3$1);N" ((1 . 2)))
    ("@1$2=vmean(@1$1..@3$1);N" ((1 . 2)))
    ("@1$2=vmin(@1$1..@3$1);N" ((1 . 2)))
    ("@1$2=vmax(@1$1..@3$1);N" ((1 . 2)))
    ("@1$2=vcount(@1$1..@3$1);N" ((1 . 2)))
    ("@1$2=2.5;%.0f" ((1 . 2)))
    ("@1$2=3.5;%.0f" ((1 . 2)))
    ("@1$2=-2.5;%.0f" ((1 . 2)))
    ("@1$2=1.25;%.1f" ((1 . 2)))
    ("@1$2=1.35;%.1f" ((1 . 2)))
    ("@1$2=2;%.2f" ((1 . 2)))
    ("@1$2=-0.0001;%.0f" ((1 . 2)))
    ("@1$2=-0.0001;%.2f" ((1 . 2)))
    ("@1$2=1/8;%.2f" ((1 . 2)))
    ("@1$2=3/8;%.2f" ((1 . 2)))
    ("@1$2=-1/8;%.2f" ((1 . 2)))
    ("@1$2=2;EN%.2f" ((1 . 2)))
    ("@1$2=2;N%.2f" ((1 . 2)))
    ("@1$2=@1$1" ((1 . 2)))
    ("@5$4=vsum(@2$1..@4$3)" ((5 . 4)))
    ("@5$4=vmean(@2$1..@4$3)" ((5 . 4)))
    ("@5$4=vmin(@2$1..@4$3)" ((5 . 4)))
    ("@5$4=vmax(@2$1..@4$3)" ((5 . 4)))
    ("@5$4=vcount(@2$1..@4$3)" ((5 . 4)))
    ("@1$2=@1$1..@2$1" ((1 . 2)))
    ("@1$2=(@1$1..@2$1)+1" ((1 . 2)))
    ("@1$2=-@1$1..@2$1" ((1 . 2))))
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

(defun org-fixture--export-relative-path-p (path)
  (and (stringp path)
       (string-match-p "\\`[[:alnum:]_.-]+\\(?:/[[:alnum:]_.-]+\\)*\\'" path)
       (let ((components (split-string path "/" nil)))
         (and components
              (not (member "." components))
              (not (member ".." components))))))

(defun org-fixture--read-export-manifest (manifest corpus-directory)
  (unless (file-exists-p manifest)
    (error "Export manifest does not exist"))
  (let ((coding-system-for-read 'utf-8-unix)
        (auto-coding-functions nil)
        (enable-local-variables nil)
        (inhibit-local-variables t)
        (seen-header nil)
        (seen-cases (make-hash-table :test #'equal))
        (rows nil))
    (with-temp-buffer
      (insert-file-contents manifest)
      (dolist (line (split-string (buffer-string) "\n" t))
        (cond
         ((string-prefix-p "#" line) nil)
         ((not seen-header)
          (unless (equal line "case\tsource\tinclude")
            (error "Export manifest must begin with case/source/include columns"))
          (setq seen-header t))
         (t
          (let* ((fields (split-string line "\t" nil))
                 (case-name (nth 0 fields))
                 (source (nth 1 fields))
                 (include (nth 2 fields)))
            (unless (and (= (length fields) 3)
                         (string-match-p "\\`[a-z0-9][a-z0-9-]*\\'" case-name)
                         (org-fixture--export-relative-path-p source)
                         (or (equal include "-")
                             (org-fixture--export-relative-path-p include)))
              (error "Invalid export manifest row"))
            (when (gethash case-name seen-cases)
              (error "Duplicate export case in manifest: %s" case-name))
            (puthash case-name t seen-cases)
            (dolist (relative (delq nil (list source (unless (equal include "-") include))))
              (let* ((root (file-name-as-directory (file-truename corpus-directory)))
                     (path (expand-file-name relative root)))
                (unless (and (file-exists-p path)
                             (string-prefix-p root (file-truename path)))
                  (error "Export fixture input is missing or escapes the corpus"))))
            (push (list case-name source (unless (equal include "-") include)) rows))))))
    (unless seen-header
      (error "Export manifest has no header"))
    (unless rows
      (error "Export manifest contains no cases"))
    (nreverse rows)))

(defun org-fixture--read-export-source (file)
  (let ((coding-system-for-read 'utf-8-unix)
        (auto-coding-functions nil)
        (enable-local-variables nil)
        (inhibit-local-variables t))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun org-fixture--export-include-directives (source)
  (let ((lines (split-string source "\n" nil))
        directives)
    (dolist (line lines)
      (when (string-match-p "\\`[ \t]*#\\+INCLUDE:" line)
        (unless (string-match "\\`[ \t]*#\\+INCLUDE:[ \t]+\"\\([[:alnum:]_.-]+\\(?:/[[:alnum:]_.-]+\\)*\\)\"[ \t]*\\'" line)
          (error "Export oracle only supports a quoted literal include path"))
        (push (match-string 1 line) directives)))
    (nreverse directives)))

(defun org-fixture--copy-export-input (relative corpus-directory temporary-directory)
  (let* ((source (expand-file-name relative corpus-directory))
         (target (expand-file-name relative temporary-directory))
         (root (file-name-as-directory (file-truename corpus-directory))))
    (unless (and (file-exists-p source)
                 (string-prefix-p root (file-truename source)))
      (error "Export oracle input is missing or escapes the corpus"))
    (make-directory (file-name-directory target) t)
    (copy-file source target t)
    target))

(defun org-fixture--export-buffer (source-file backend)
  (let ((default-directory (file-name-directory source-file)))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix)
            (auto-coding-functions nil)
            (enable-local-variables nil)
            (inhibit-local-variables t)
            (org-export-use-babel nil)
            (org-export-time-stamp-file nil))
        (insert-file-contents source-file)
        (setq buffer-file-name source-file)
        (org-mode)
        (substring-no-properties (org-export-as backend nil nil nil nil))))))

(defun org-fixture--export-output-extension (backend)
  (or (car (cdr (assq backend org-fixture--export-backends)))
      (error "Unknown export fixture backend")))

(defun org-fixture--export-output-backend (backend)
  (or (cdr (cdr (assq backend org-fixture--export-backends)))
      (error "Unknown export fixture backend")))

(defun org-fixture--file-sha256 (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun org-fixture--export-metadata (backend source include source-sha include-sha)
  (let ((metadata
         (format (concat
                  "emacs=%s\norg=%s\ngenerator-mode=export-oracle-v1\n"
                  "backend=%s\nencoding=UTF-8\n"
                  "reference-time=2000-01-01T00:00:00Z\ntime-zone=UTC\n"
                  "dst-policy=compatible\nunicode-width-data=Unicode-17.0.0\n"
                  "source=%s\nsource-sha256=%s\n"
                  "include-source=%s\ninclude-source-sha256=%s\n")
                 emacs-version (org-version) backend
                 source source-sha (or include "none") (or include-sha "none"))))
    (if (eq backend 'gfm)
        (concat metadata
                (format (concat
                         "ox-gfm-version=%s\nox-gfm-repository=%s\n"
                         "ox-gfm-commit=%s\nox-gfm-source-sha256=%s\n")
                        org-fixture--ox-gfm-version
                        org-fixture--ox-gfm-repository
                        org-fixture--ox-gfm-commit
                        org-fixture--ox-gfm-source-sha256))
      metadata)))

(defun org-fixture--verify-ox-gfm-pin ()
  (require 'ox-gfm)
  (let ((file (locate-library "ox-gfm.el")))
    (unless file
      (error "Unable to locate ox-gfm source"))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (unless (and (re-search-forward "^;; Package-Version: \\(.*\\)$" nil t)
                   (equal (match-string 1) org-fixture--ox-gfm-version))
        (error "Export oracle requires the pinned ox-gfm version"))
      (unless (equal (org-fixture--file-sha256 file)
                     org-fixture--ox-gfm-source-sha256)
        (error "Export oracle ox-gfm source does not match the pinned commit")))))

(defun org-fixture--check-export-output (text temporary-directory)
  (when (or (string-match-p (regexp-quote temporary-directory) text)
            (and (not (equal (expand-file-name "~") "/"))
                 (string-match-p (regexp-quote (expand-file-name "~")) text)))
    (error "Generated export unexpectedly contains a local filesystem path"))
  text)

(defun org-fixture--write-export-file (file text)
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent)))

(defun org-fixture--generate-export-case (case corpus-directory expected-directory)
  (let* ((case-name (nth 0 case))
         (source-relative (nth 1 case))
         (include-relative (nth 2 case))
         (source (expand-file-name source-relative corpus-directory))
         (source-text (org-fixture--read-export-source source))
         (include-directives (org-fixture--export-include-directives source-text)))
    (unless (equal include-directives (if include-relative (list include-relative) nil))
      (error "Export manifest does not match the source include directive"))
    (let* ((temporary-directory (file-name-as-directory
                                 (make-temp-file "org-fixture-export-" t)))
           (temporary-source (org-fixture--copy-export-input
                              source-relative corpus-directory temporary-directory))
           (source-sha (org-fixture--file-sha256 source))
           (include-sha (when include-relative
                          (org-fixture--file-sha256
                           (expand-file-name include-relative corpus-directory)))))
      (unwind-protect
          (progn
            (when include-relative
              (org-fixture--copy-export-input
               include-relative corpus-directory temporary-directory))
            (dolist (backend '(html gfm ascii))
              (let* ((backend-name (symbol-name backend))
                     (backend-directory (expand-file-name backend-name expected-directory))
                     (output-file (expand-file-name
                                   (format "%s.%s" case-name
                                           (org-fixture--export-output-extension backend))
                                   backend-directory))
                     (metadata-file (expand-file-name
                                     (format "%s.meta" case-name) backend-directory))
                     (text (org-fixture--check-export-output
                            (org-fixture--export-buffer
                             temporary-source
                             (org-fixture--export-output-backend backend))
                            temporary-directory)))
                (org-fixture--write-export-file output-file text)
                (org-fixture--write-export-file
                 metadata-file
                 (org-fixture--export-metadata backend source-relative include-relative
                                               source-sha include-sha))
                (princ (format "Wrote %s/%s.%s and %s/%s.meta\n"
                               backend-name case-name
                               (org-fixture--export-output-extension backend)
                               backend-name case-name)))))
        (delete-directory temporary-directory t)))))

(defun org-fixture-generate-export-oracle (corpus-directory manifest expected-directory)
  "Generate full Org export outputs for the literal export oracle manifest."
  (unless (file-directory-p corpus-directory)
    (error "Export oracle corpus directory does not exist"))
  (require 'ox-html)
  (require 'ox-ascii)
  (org-fixture--verify-ox-gfm-pin)
  (let ((cases (org-fixture--read-export-manifest manifest corpus-directory))
        (org-export-use-babel nil))
    (dolist (case cases)
      (org-fixture--generate-export-case case corpus-directory expected-directory))
    (princ (format "Generated %d export oracle cases for html/gfm/ascii.\n"
                   (length cases)))
    (princ (format "Emacs %s; Org %s; ox-gfm %s\n"
                   emacs-version (org-version) org-fixture--ox-gfm-version))))

(defun org-fixture--statistics-cookie-values ()
  (let (values)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[[0-9]+\\(%\\|/[0-9]+\\)\\]" nil t)
        (push (match-string-no-properties 0) values)))
    (nreverse values)))

(defun org-fixture-generate-statistics-cookies (source-directory expected-directory)
  "Update the first statistics cookie in each source file with Org's API."
  (unless (and (equal emacs-version "31.1")
               (equal (org-version) "9.8.10"))
    (error "Statistics cookie fixtures require Emacs 31.1 and Org 9.8.10"))
  (unless (file-directory-p source-directory)
    (error "Source directory does not exist: %s" source-directory))
  (let ((files (sort (directory-files-recursively source-directory "\\.org\\'")
                     #'string<)))
    (unless files (error "No .org files found under %s" source-directory))
    (dolist (file files)
      (let ((target-heading nil)
            (values nil))
        (let* ((relative (file-relative-name file source-directory))
               (output (expand-file-name relative expected-directory))
               (content
                (with-temp-buffer
                  (org-fixture--read-buffer file)
                  (goto-char (point-min))
                  (unless (looking-at "\\* ")
                    (error "Statistics cookie fixture must start with a root heading: %s"
                           relative))
                  (let ((root-count 0))
                    (while (re-search-forward "^\\* " nil t)
                      (setq root-count (1+ root-count)))
                    (unless (= root-count 1)
                      (error "Expected one root heading in %s, found %d"
                             relative root-count)))
                  (goto-char (point-min))
                  (unless (re-search-forward "\\[[0-9]+\\(%\\|/[0-9]+\\)\\]" nil t)
                    (error "No statistics cookie found in %s" relative))
                  (setq target-heading
                        (save-excursion
                          (org-back-to-heading t)
                          (org-get-heading t t t t)))
                  (org-update-statistics-cookies nil)
                  (setq values (org-fixture--statistics-cookie-values))
                  (buffer-string))))
          (make-directory (file-name-directory output) t)
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region content nil output nil 'silent))
          (princ (format "Wrote %s at heading %S; statistics cookies %S\n"
                         relative target-heading values)))))
    (princ (format "Generated %d statistics cookie fixtures.\n"
                   (length files)))
    (princ (format "Emacs %s; Org %s\n" emacs-version (org-version)))))

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
     ((and (= (length arguments) 4) (equal (car arguments) "--h1-clock"))
      (org-fixture-generate-h1-clock
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
     ((and (= (length arguments) 4) (equal (car arguments) "--export-oracle"))
      (org-fixture-generate-export-oracle
       (expand-file-name (nth 1 arguments))
       (expand-file-name (nth 2 arguments))
       (expand-file-name (nth 3 arguments))))
     ((and (= (length arguments) 3)
           (equal (car arguments) "--statistics-cookies"))
      (org-fixture-generate-statistics-cookies
       (expand-file-name (nth 1 arguments))
       (expand-file-name (nth 2 arguments))))
     ((= (length arguments) 2)
      (org-fixture-generate (expand-file-name (nth 0 arguments))
                            (expand-file-name (nth 1 arguments))))
     (t
      (error (concat
              "Usage: emacs --batch -Q --script scripts/generate-org-fixtures.el CORPUS-DIR EXPECTED-DIR\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --g2-clocktable CORPUS-DIR CASES-FILE EXPECTED-FILE\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --g1-clock CORPUS-DIR CASES-FILE EXPECTED-FILE\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --h1-clock CORPUS-DIR CASES-FILE EXPECTED-FILE\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --include-effective-tags CORPUS-DIR EXPECTED-DIR\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --table-calc CORPUS-DIR EXPECTED-DIR\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --export-oracle CORPUS-DIR MANIFEST EXPECTED-DIR\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --statistics-cookies SOURCE-DIR EXPECTED-DIR\n"
              "   or: emacs --batch -Q --script scripts/generate-org-fixtures.el --check-types CORPUS-DIR EXPECTED-DIR"))))))

;;; Behavior agenda oracle action

(defun org-fixture--agenda-read-case (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((case (read (current-buffer))))
      (condition-case nil
          (progn
            (read (current-buffer))
            (error "Expected exactly one agenda case in %s" file))
        (end-of-file nil))
      case)))

(defun org-fixture--agenda-parse-date (text)
  (unless (and (stringp text)
               (string-match
                "\\`\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\'"
                text))
    (error "Invalid agenda fixture date: %s" text))
  (let* ((year (string-to-number (match-string 1 text)))
         (month (string-to-number (match-string 2 text)))
         (day (string-to-number (match-string 3 text)))
         (date (list month day year))
         (absolute (calendar-absolute-from-gregorian date)))
    (unless (equal date (calendar-gregorian-from-absolute absolute))
      (error "Invalid agenda fixture date: %s" text))
    date))

(defun org-fixture--agenda-parse-reference-time (text)
  (unless (and (stringp text)
               (string-match
                "\\`\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)T\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\+00:00\\'"
                text))
    (error "Agenda fixture reference-time must be UTC ISO-8601: %s" text))
  (let* ((date-text (match-string 1 text))
         (hour (string-to-number (match-string 2 text)))
         (minute (string-to-number (match-string 3 text)))
         (second (string-to-number (match-string 4 text)))
         (date (org-fixture--agenda-parse-date date-text)))
    (encode-time second minute hour (nth 1 date) (car date) (nth 2 date) t)))

(defun org-fixture--agenda-iso-date (date)
  (let ((date (if (numberp date)
                  (calendar-gregorian-from-absolute date)
                date)))
    (and (consp date)
         (format "%04d-%02d-%02d" (nth 2 date) (car date) (nth 1 date)))))

(defun org-fixture--agenda-kind (type)
  (cond
   ((member type '("deadline" "upcoming-deadline")) :deadline)
   ((member type '("scheduled" "past-scheduled")) :scheduled)
   ((member type '("timestamp" "block")) :timestamp)
   (t nil)))

(defun org-fixture--agenda-row-data (row date kind match-function properties)
  (let* ((marker (and row (get-text-property 0 'org-hd-marker row)))
         (type (and row (get-text-property 0 'type row)))
         (kind (or kind (org-fixture--agenda-kind type))))
    (unless marker
      (error "Org agenda row has no heading marker"))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let* ((selected-day (and date
                                  (calendar-absolute-from-gregorian
                                   (org-fixture--agenda-parse-date date))))
               (timestamp-day (and row (get-text-property 0 'ts-date row)))
               (row-time (and row (get-text-property 0 'dotime row)))
               (scheduled-timestamp (org-entry-get nil "SCHEDULED"))
               (deadline-timestamp (org-entry-get nil "DEADLINE"))
               (timestamp
                (or (and (stringp row-time) (substring-no-properties row-time))
                    (pcase kind
                      (:deadline deadline-timestamp)
                      (:scheduled scheduled-timestamp)
                      (_ nil))))
               (warning-days (and (eq kind :deadline) timestamp
                                  (org-get-wdays timestamp)))
               (delay-days (and (eq kind :scheduled) timestamp
                                (org-get-wdays timestamp t)))
               (tags (org-get-tags nil nil))
               (headline (substring-no-properties (org-get-heading t t t t)))
               (path (mapcar #'substring-no-properties (org-get-outline-path t)))
               (todo (or (and row (get-text-property 0 'todo-state row))
                         (org-get-todo-state)))
               (property-values
                (mapcar (lambda (name)
                          (cons name (org-entry-get nil name t)))
                        properties))
               (match-p (and match-function
                             (funcall match-function
                                      todo tags
                                      (org-reduced-level (org-outline-level)))))
               (warning-state
                (and (eq kind :deadline)
                     (cond
                      ((equal type "upcoming-deadline") :prewarning)
                      ((and selected-day timestamp-day
                            (= selected-day timestamp-day)) :due)
                      ((and selected-day timestamp-day
                            (< selected-day timestamp-day)) :prewarning)
                      ((and selected-day timestamp-day
                            (> selected-day timestamp-day)) :overdue))))
               (delay-state
                (and (eq kind :scheduled) delay-days (> delay-days 0)
                     (equal type "past-scheduled") :delayed)))
          (list :date date
                :kind kind
                :org-type type
                :org-date (and row
                               (org-fixture--agenda-iso-date
                                (get-text-property 0 'date row)))
                :timestamp timestamp
                :timestamp-date (and timestamp-day
                                     (org-fixture--agenda-iso-date timestamp-day))
                :scheduled-timestamp scheduled-timestamp
                :deadline-timestamp deadline-timestamp
                :headline headline
                :heading-path path
                :warning-state warning-state
                :warning-days warning-days
                :scheduled-delay-state delay-state
                :scheduled-delay-days delay-days
                :todo-state todo
                :effective-tags (mapcar #'substring-no-properties tags)
                :property-values property-values
                :habit-p (and row (get-text-property 0 'org-habit-p row) t)
                :source-line (line-number-at-pos (marker-position marker))
                :match-p (and match-p t)))))))

(defun org-fixture--agenda-matcher (match)
  (when match
    (unless (stringp match)
      (error "Agenda fixture match must be a string or nil"))
    (let ((org--matcher-tags-todo-only nil))
      (cdr (org-make-tags-matcher match)))))

(defun org-fixture--agenda-entry-kind-order (entry)
  (or (cl-position (plist-get entry :kind)
                   '(:deadline :scheduled :timestamp :todo))
      4))

(defun org-fixture--agenda-sort-entries (entries)
  ;; Org's display order can differ from the stable order promised by the API.
  (cl-stable-sort
   entries
   (lambda (left right)
     (let ((left-date (or (plist-get left :date) ""))
           (right-date (or (plist-get right :date) ""))
           (left-kind (org-fixture--agenda-entry-kind-order left))
           (right-kind (org-fixture--agenda-entry-kind-order right))
           (left-line (or (plist-get left :source-line) 0))
           (right-line (or (plist-get right :source-line) 0)))
       (cond
        ((not (equal left-date right-date))
         (string< left-date right-date))
        ((/= left-kind right-kind)
         (< left-kind right-kind))
        (t (< left-line right-line)))))))

(defun org-fixture--agenda-render-snapshot (file snapshot properties match-function)
  (let* ((reference-time (plist-get snapshot :reference-time))
         (reference (org-fixture--agenda-parse-reference-time reference-time))
         (reference-date (substring reference-time 0 10))
         (start-date (plist-get snapshot :start-date))
         (end-date (plist-get snapshot :end-date))
         (start-day (calendar-absolute-from-gregorian
                     (org-fixture--agenda-parse-date start-date)))
         (end-day (calendar-absolute-from-gregorian
                   (org-fixture--agenda-parse-date end-date)))
         (warning-days (plist-get snapshot :warning-days))
         (scheduled-delay (plist-get snapshot :scheduled-delay))
         (future-mode (pcase (plist-get snapshot :future-repeats)
                        (:all t)
                        (:next 'next)
                        (:none nil)
                        (_ (error "Invalid future-repeats mode in agenda case"))))
         entries)
    (unless (<= start-day end-day)
      (error "Agenda fixture range start exceeds end"))
    (unless (and (integerp warning-days) (>= warning-days 0)
                 (integerp scheduled-delay) (>= scheduled-delay 0))
      (error "Agenda fixture warning/delay values must be non-negative integers"))
    (let ((calendar-time-zone 0)
          (org-deadline-warning-days warning-days)
          (org-scheduled-delay-days scheduled-delay)
          (org-agenda-show-future-repeats future-mode)
          (org-agenda-include-inactive-timestamps nil)
          (org-agenda-show-inherited-tags t)
          (org-agenda-use-tag-inheritance '(agenda))
          (org-use-tag-inheritance t)
          (org-agenda-skip-scheduled-if-deadline-is-shown nil)
          (org-agenda-skip-timestamp-if-deadline-is-shown nil)
          (org-agenda-skip-timestamp-if-done nil)
          (org-agenda-entry-types '(:deadline :scheduled :timestamp)))
      (cl-letf (((symbol-function 'current-time)
                 (lambda () reference))
                ((symbol-function 'current-time-string)
                 (lambda (&optional _time _zone)
                   (format-time-string "%a %b %e %T %Y" reference t)))
                ((symbol-function 'org-today)
                 (lambda ()
                   (calendar-absolute-from-gregorian
                    (org-fixture--agenda-parse-date reference-date)))))
        (let ((day start-day))
          (while (<= day end-day)
            (let ((selected-date (org-fixture--agenda-iso-date day)))
              (dolist (row (org-agenda-get-day-entries
                            file (calendar-gregorian-from-absolute day)
                            :deadline :scheduled :timestamp))
                (push (org-fixture--agenda-row-data
                       row selected-date nil match-function properties)
                      entries)))
            (setq day (1+ day))))))
    (append (list :name (plist-get snapshot :name)
                  :reference-time reference-time
                  :start-date start-date
                  :end-date end-date
                  :warning-days warning-days
                  :scheduled-delay scheduled-delay
                  :future-repeats (plist-get snapshot :future-repeats)
                  :entries (org-fixture--agenda-sort-entries
                            (nreverse entries))))))

(defun org-fixture--agenda-map-entries (file match properties)
  (let ((match-function (org-fixture--agenda-matcher match)))
    (with-current-buffer (org-get-agenda-file-buffer file)
      (org-map-entries
       (lambda ()
         (let ((row (org-add-props " " nil
                      'org-hd-marker (copy-marker (point))
                      'type nil 'date nil 'ts-date nil
                      'tags (org-get-tags nil nil)
                      'todo-state (org-get-todo-state)
                      'org-habit-p (and (fboundp 'org-is-habit-p)
                                        (org-is-habit-p)))))
           (org-fixture--agenda-row-data
            row nil nil match-function properties)))
       match 'file))))

(defun org-fixture--agenda-todo-case (file todo-case properties)
  (let* ((match (plist-get todo-case :match))
         (match-function (org-fixture--agenda-matcher match))
         (rows (with-current-buffer (org-get-agenda-file-buffer file)
                 (org-agenda-get-todos)))
         (entries
          (mapcar (lambda (row)
                    (org-fixture--agenda-row-data
                     row nil :todo match-function properties))
                  rows)))
    (list :name (plist-get todo-case :name)
          :match match
          :entries (if match-function
                       (cl-remove-if-not (lambda (entry)
                                           (plist-get entry :match-p))
                                         entries)
                     entries))))

(defun org-fixture--agenda-pretty-data (data)
  (require 'pp)
  (pp-to-string (car (read-from-string (org-fixture--cl-value data)))))

(defun org-fixture--agenda-render-case (case-file corpus-directory)
  (let* ((case (org-fixture--agenda-read-case case-file))
         (source (plist-get case :source))
         (properties (plist-get case :properties))
         (source-file (and (stringp source)
                           (not (file-name-absolute-p source))
                           (expand-file-name source corpus-directory)))
         (match (plist-get case :match))
         (match-function (org-fixture--agenda-matcher match))
         (match-cases (plist-get case :match-cases))
         (todo-cases (plist-get case :todo-cases)))
    (unless (and source-file (file-exists-p source-file)
                 (listp properties) (cl-every #'stringp properties))
      (error "Invalid agenda fixture case: %s" case-file))
    (require 'org-habit)
    (let ((org-habit-show-habits t)
          (org-habit-show-habits-only-for-today nil)
          (org-agenda-show-inherited-tags t)
          (org-agenda-use-tag-inheritance '(agenda))
          (org-use-tag-inheritance t))
      (let* ((snapshots
              (mapcar (lambda (snapshot)
                        (org-fixture--agenda-render-snapshot
                         source-file snapshot properties match-function))
                      (plist-get case :snapshots)))
             (matching
              (mapcar (lambda (match-case)
                        (list :name (plist-get match-case :name)
                              :match (plist-get match-case :match)
                              :entries
                              (org-fixture--agenda-map-entries
                               source-file (plist-get match-case :match) properties)))
                      match-cases))
             (todos
              (mapcar (lambda (todo-case)
                        (org-fixture--agenda-todo-case
                         source-file todo-case properties))
                      todo-cases))
             (data (list :source source
                         :emacs-version emacs-version
                         :org-version (org-version)
                         :snapshots snapshots
                         :match-cases matching
                         :todo-cases todos)))
        (org-fixture--check-output
         (concat
          (format "; Generated from Org agenda APIs with Emacs %s and Org %s.\n"
                  emacs-version (org-version))
          "; Fixed reference times and UTC calendar dates come from the case data.\n"
          "; Agenda and TODO rows use Org APIs; the fixture's diary sexp is not evaluated.\n"
          (org-fixture--agenda-pretty-data data)))))))

(defun org-fixture-generate-agenda (corpus-directory case-directory expected-directory)
  (unless (file-directory-p corpus-directory)
    (error "Agenda corpus directory does not exist: %s" corpus-directory))
  (unless (file-directory-p case-directory)
    (error "Agenda case directory does not exist: %s" case-directory))
  (let ((files (sort (directory-files-recursively case-directory "\\.case\\'")
                     #'string<)))
    (unless files
      (error "No agenda .case files found under %s" case-directory))
    (dolist (file files)
      (let* ((relative (concat (file-relative-name file case-directory) ".sexp"))
             (output (expand-file-name relative expected-directory))
             (content (org-fixture--agenda-render-case file corpus-directory)))
        (make-directory (file-name-directory output) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region content nil output nil 'silent))
        (princ (format "Wrote %s\n" relative))))
    (princ (format "Generated %d agenda fixture files.\n" (length files)))
    (princ (format "Emacs %s; Org %s\n" emacs-version (org-version)))))

(defun org-fixture--agenda-main-around (original)
  (let ((arguments (if (equal (car command-line-args-left) "--")
                       (cdr command-line-args-left)
                     command-line-args-left)))
    (if (equal (car arguments) "--agenda")
        (progn
          (setq command-line-args-left nil)
          (unless (= (length arguments) 4)
            (error (concat
                    "Usage: emacs --batch -Q --script scripts/generate-org-fixtures.el "
                    "--agenda CORPUS-DIR CASE-DIR EXPECTED-DIR")))
          (org-fixture-generate-agenda
           (expand-file-name (nth 1 arguments))
           (expand-file-name (nth 2 arguments))
           (expand-file-name (nth 3 arguments))))
      (funcall original))))

(advice-add 'org-fixture--main :around #'org-fixture--agenda-main-around)

;;; T5b stable table sort fixtures

(defconst org-fixture--t5b-required-metadata
  '("T5b fixture metadata: GNU Emacs 31.1; Org 9.8.10"
    "Generator mode: org-table-sort-lines"
    "Backend: none"
    "Encoding: UTF-8"))

(defun org-fixture--t5b-metadata (file key)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (goto-char (point-min))
    (unless (re-search-forward
             (concat "^# " (regexp-quote key) ": \\(.+\\)$") nil t)
      (error "Missing T5b metadata %s in %s" key file))
    (string-trim (match-string 1))))

(defun org-fixture--t5b-positive-integer (file key)
  (let ((text (org-fixture--t5b-metadata file key)))
    (unless (string-match-p "\\`[1-9][0-9]*\\'" text)
      (error "Invalid T5b integer metadata %s in %s" key file))
    (string-to-number text)))

(defun org-fixture--t5b-hline-p ()
  (looking-at "^[ \t]*|[-+| \t]+|[ \t]*$"))

(defun org-fixture--t5b-row-position (file row column)
  (goto-char (point-min))
  (unless (re-search-forward "^[ \t]*|" nil t)
    (error "No Org pipe table in %s" file))
  (beginning-of-line)
  (let ((remaining row)
        found)
    (while (and (> remaining 0) (looking-at "^[ \t]*|"))
      (unless (org-fixture--t5b-hline-p)
        (setq remaining (1- remaining))
        (when (= remaining 0)
          (setq found (point))))
      (unless found
        (forward-line 1)))
    (unless found
      (error "T5b row %d is outside the table in %s" row file))
    (goto-char found)
    (let ((line-position (line-beginning-position)))
      (org-table-goto-column column)
      (unless (and (org-at-table-p)
                   (= (org-table-current-column) column))
        (error "T5b column %d is outside row %d in %s" column row file))
      line-position)))

(defun org-fixture--t5b-render-file (file)
  (dolist (metadata org-fixture--t5b-required-metadata)
    (unless (with-temp-buffer
              (let ((coding-system-for-read 'utf-8-unix))
                (insert-file-contents file))
              (goto-char (point-min))
              (re-search-forward (concat "^# " (regexp-quote metadata) "$") nil t))
      (error "Missing T5b fixture metadata %S in %s" metadata file)))
  (let* ((type (org-fixture--t5b-metadata file "Sort type"))
         (order (org-fixture--t5b-metadata file "Order"))
         (sort-type
          (cond
           ((and (equal type "lexical") (equal order "ascending")) ?a)
           ((and (equal type "lexical") (equal order "descending")) ?A)
           ((and (equal type "numeric") (equal order "ascending")) ?n)
           ((and (equal type "numeric") (equal order "descending")) ?N)
           (t (error "Invalid T5b sort type/order in %s" file))))
         (column (org-fixture--t5b-positive-integer file "Column"))
         (start (org-fixture--t5b-positive-integer file "Start"))
         (end (org-fixture--t5b-positive-integer file "End")))
    (unless (<= start end)
      (error "Invalid T5b row range in %s" file))
    (with-temp-buffer
      (org-fixture--read-buffer file)
      (let ((start-position (org-fixture--t5b-row-position file start column))
            (end-position (org-fixture--t5b-row-position file end column)))
        ;; Org moves to region-beginning before checking it again, so keep point there.
        (set-mark end-position)
        (goto-char start-position)
        (setq-local transient-mark-mode t)
        (setq mark-active t)
        (cl-letf (((symbol-function 'read-number)
                   (lambda (&rest _arguments) column)))
          (org-table-sort-lines nil sort-type nil nil t)))
      (org-fixture--check-output (buffer-string)))))

(defun org-fixture-generate-table-sort (corpus-directory expected-directory)
  (unless (and (equal emacs-version "31.1")
               (equal (org-version) "9.8.10"))
    (error "T5b oracle requires Emacs 31.1 and Org 9.8.10; found %s and %s"
           emacs-version (org-version)))
  (unless (file-directory-p corpus-directory)
    (error "T5b corpus directory does not exist: %s" corpus-directory))
  (let ((files (sort (directory-files-recursively corpus-directory "\\.org\\'")
                     #'string<)))
    (unless files
      (error "No T5b table-sort fixtures found under %s" corpus-directory))
    (dolist (file files)
      (let* ((relative (file-relative-name file corpus-directory))
             (output (expand-file-name relative expected-directory))
             (content (org-fixture--t5b-render-file file)))
        (make-directory (file-name-directory output) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region content nil output nil 'silent))
        (princ (format "Wrote %s\n" relative))))
    (princ (format "Generated %d T5b table-sort fixtures.\n" (length files)))
    (princ (format "Emacs %s; Org %s\n" emacs-version (org-version)))))

(defun org-fixture--t5b-main-around (original)
  (let ((arguments (if (equal (car command-line-args-left) "--")
                       (cdr command-line-args-left)
                     command-line-args-left)))
    (if (equal (car arguments) "--table-sort")
        (progn
          (setq command-line-args-left nil)
          (unless (= (length arguments) 3)
            (error (concat
                    "Usage: emacs --batch -Q --script scripts/generate-org-fixtures.el "
                    "--table-sort CORPUS-DIR EXPECTED-DIR")))
          (org-fixture-generate-table-sort
           (expand-file-name (nth 1 arguments))
           (expand-file-name (nth 2 arguments))))
      (funcall original))))

(advice-add 'org-fixture--dispatch-main :around #'org-fixture--t5b-main-around)

(defun org-fixture--t5b-render-file-without-range (file)
  (dolist (metadata org-fixture--t5b-required-metadata)
    (unless (with-temp-buffer
              (let ((coding-system-for-read 'utf-8-unix))
                (insert-file-contents file))
              (goto-char (point-min))
              (re-search-forward (concat "^# " (regexp-quote metadata) "$") nil t))
      (error "Missing T5b fixture metadata %S in %s" metadata file)))
  (let* ((type (org-fixture--t5b-metadata file "Sort type"))
         (order (org-fixture--t5b-metadata file "Order"))
         (sort-type
          (cond
           ((and (equal type "lexical") (equal order "ascending")) ?a)
           ((and (equal type "lexical") (equal order "descending")) ?A)
           ((and (equal type "numeric") (equal order "ascending")) ?n)
           ((and (equal type "numeric") (equal order "descending")) ?N)
           (t (error "Invalid T5b sort type/order in %s" file))))
         (column (org-fixture--t5b-positive-integer file "Column")))
    (with-temp-buffer
      (org-fixture--read-buffer file)
      (org-fixture--t5b-row-position file 1 column)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _arguments) column)))
        (org-table-sort-lines nil sort-type nil nil t))
      (org-fixture--check-output (buffer-string)))))

(defun org-fixture--t5b-render-file-around (original file)
  (if (and (equal (org-fixture--t5b-metadata file "Start") "omitted")
           (equal (org-fixture--t5b-metadata file "End") "omitted"))
      (org-fixture--t5b-render-file-without-range file)
    (funcall original file)))

(advice-add 'org-fixture--t5b-render-file :around
            #'org-fixture--t5b-render-file-around)

(defun org-fixture--h2-clocktable-read-cases (file)
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

(defun org-fixture--h2-clocktable-validate-cases (cases)
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (case cases)
      (unless (and (proper-list-p case)
                   (eq (car-safe case) :id)
                   (stringp (plist-get case :id))
                   (not (gethash (plist-get case :id) ids))
                   (stringp (plist-get case :source))
                   (integerp (plist-get case :total-minutes))
                   (>= (plist-get case :total-minutes) 0)
                   (proper-list-p (plist-get case :rows)))
        (error "Invalid H2 clocktable case: %S" case))
      (puthash (plist-get case :id) t ids)
      (dolist (row (plist-get case :rows))
        (unless (and (proper-list-p row)
                     (integerp (plist-get row :level))
                     (> (plist-get row :level) 0)
                     (stringp (plist-get row :title))
                     (or (null (plist-get row :tags))
                         (and (proper-list-p (plist-get row :tags))
                              (cl-every #'stringp (plist-get row :tags))))
                     (integerp (plist-get row :minutes))
                     (>= (plist-get row :minutes) 0))
          (error "Invalid H2 clocktable row: %S" row)))
      (unless (and (plist-member case :source)
                   (plist-member case :total-minutes)
                   (plist-member case :rows))
        (error "Missing H2 clocktable case property: %S" case)))
    cases))

(defun org-fixture--h2-clocktable-table (case)
  (let* ((entries
          (mapcar (lambda (row)
                    (list (plist-get row :level)
                          (plist-get row :title)
                          (plist-get row :tags)
                          nil
                          (plist-get row :minutes)
                          nil))
                  (plist-get case :rows)))
         (tables (list (list (plist-get case :source)
                             (plist-get case :total-minutes)
                             entries)))
         (params (org-combine-plists org-clocktable-defaults '(:header ""))))
    (with-temp-buffer
      (org-mode)
      (org-clocktable-write-default (point-min) tables params)
      (buffer-string))))

(defun org-fixture--h2-clocktable-updated-source (source table)
  (with-temp-buffer
    (insert source)
    (let* ((case-fold-search t)
           (tree (org-element-parse-buffer))
           (block
            (org-element-map tree 'dynamic-block
              (lambda (element)
                (when (equal (org-element-property :block-name element)
                             "clocktable")
                  element))
              nil t)))
      (unless block
        (error "No clocktable dynamic block in H2 fixture source"))
      (goto-char (org-element-property :begin block))
      (forward-line 1)
      (let ((start (point))
            end)
        (unless (re-search-forward "^[ \t]*#\\+END:[ \t]*$" nil t)
          (error "Clocktable dynamic block has no #+END line"))
        (setq end (line-beginning-position))
        (concat (buffer-substring-no-properties (point-min) start)
                table
                (buffer-substring-no-properties end (point-max)))))))

(defun org-fixture--h2-clocktable-render-case (case corpus-directory)
  (let* ((source-file (expand-file-name (plist-get case :source)
                                        corpus-directory))
         (_ (unless (file-exists-p source-file)
              (error "H2 clocktable corpus file does not exist: %s"
                     (plist-get case :source))))
         (source
          (with-temp-buffer
            (let ((coding-system-for-read 'utf-8-unix))
              (insert-file-contents source-file))
            (buffer-string)))
         (table (org-fixture--h2-clocktable-table case)))
    (list :id (plist-get case :id)
          :org-clocktable-body table
          :org-updated-document
          (org-fixture--h2-clocktable-updated-source source table))))

(defun org-fixture-generate-h2-clocktable
    (corpus-directory cases-file expected-file)
  (unless (file-directory-p corpus-directory)
    (error "H2 clocktable corpus directory does not exist: %s" corpus-directory))
  (let* ((cases (org-fixture--h2-clocktable-validate-cases
                 (org-fixture--h2-clocktable-read-cases cases-file)))
         (results (mapcar (lambda (case)
                            (org-fixture--h2-clocktable-render-case
                             case corpus-directory))
                          cases))
         (content
          (org-fixture--check-output
           (concat
            (format "; Generated from org-clocktable-write-default with Emacs %s and Org %s.\n"
                    emacs-version (org-version))
            "; Policy fixture: omit Org's volatile current-time caption for pure H2 editing.\n"
            "; Formatting uses org-clocktable-defaults; all supplied rows are retained.\n"
            "(:cases\n ("
            (mapconcat #'org-fixture--cl-value results "\n ")
            "))\n"))))
    (make-directory (file-name-directory expected-file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region content nil expected-file nil 'silent))
    (princ (format "Wrote %s\n" (file-name-nondirectory expected-file)))
    (princ (format "Generated %d H2 clocktable cases.\n" (length cases)))
    (princ (format "Emacs %s; Org %s\n" emacs-version (org-version)))))

(defun org-fixture--h2-clocktable-main-around (original)
  (let ((arguments (if (equal (car command-line-args-left) "--")
                       (cdr command-line-args-left)
                     command-line-args-left)))
    (if (equal (car arguments) "--h2-clocktable")
        (progn
          (setq command-line-args-left nil)
          (unless (= (length arguments) 4)
            (error (concat
                    "Usage: emacs --batch -Q --script scripts/generate-org-fixtures.el "
                    "--h2-clocktable CORPUS-DIR CASES-FILE EXPECTED-FILE")))
          (org-fixture-generate-h2-clocktable
           (expand-file-name (nth 1 arguments))
           (expand-file-name (nth 2 arguments))
           (expand-file-name (nth 3 arguments))))
      (funcall original))))

(advice-add 'org-fixture--main :around #'org-fixture--h2-clocktable-main-around)

;;; T3 table editing fixtures

(require 'cl-lib)
(require 'diff)

(defconst org-fixture--table-edit-required-metadata
  '("GNU Emacs 31.1; Org 9.8.10"
    "Generator mode: org-table-edit"
    "Backend: none"
    "Encoding: UTF-8"
    "Reference time: 2000-01-01T00:00:00Z (fixed; unused)"
    "Zone/DST policy: UTC; DST not applicable"))

(defun org-fixture--table-edit-read-cases (file)
  (with-temp-buffer
    (let ((read-eval nil))
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((cases (read (current-buffer))))
        (condition-case nil
            (progn
              (read (current-buffer))
              (error "Expected one T3 operations list in %s"
                     (file-name-nondirectory file)))
          (end-of-file nil))
        cases))))

(defun org-fixture--table-edit-validate-cases (cases)
  (let ((ids (make-hash-table :test #'equal))
        (actions '(:insert-row :delete-row :move-row
                   :insert-column :delete-column :move-column))
        (allowed-keys '(:id :source :action :index :values :from :to)))
    (unless (and (listp cases) cases)
      (error "T3 operations must be a non-empty list"))
    (dolist (case cases)
      (unless (and (listp case) (= (% (length case) 2) 0))
        (error "Malformed T3 operation"))
      (let ((tail case))
        (while tail
          (let ((key (pop tail)))
            (pop tail)
            (unless (memq key allowed-keys)
              (error "Unknown T3 operation key %S" key)))))
      (let ((id (plist-get case :id))
            (source (plist-get case :source))
            (action (plist-get case :action))
            (values (plist-get case :values)))
        (unless (and (stringp id)
                     (string-match-p "\\`[a-z0-9][a-z0-9-]*\\'" id)
                     (not (gethash id ids)))
          (error "Invalid or duplicate T3 operation id"))
        (puthash id t ids)
        (unless (and (stringp source)
                     (equal source (file-name-nondirectory source))
                     (string-match-p "\\.org\\'" source))
          (error "T3 source must be a corpus .org file name"))
        (unless (memq action actions)
          (error "Unsupported T3 operation action %S" action))
        (when (plist-member case :values)
          (unless (and (listp values) (cl-every #'stringp values))
            (error "T3 inserted values must be a string list")))
        (pcase action
          ((or :insert-row :insert-column)
           (unless (and (plist-member case :index)
                        (integerp (plist-get case :index))
                        (> (plist-get case :index) 0))
             (error "T3 insert operation needs a positive index")))
          ((or :delete-row :delete-column)
           (unless (and (plist-member case :index)
                        (integerp (plist-get case :index))
                        (> (plist-get case :index) 0))
             (error "T3 delete operation needs a positive index")))
          ((or :move-row :move-column)
           (unless (and (plist-member case :from)
                        (plist-member case :to)
                        (integerp (plist-get case :from))
                        (integerp (plist-get case :to))
                        (> (plist-get case :from) 0)
                        (> (plist-get case :to) 0)
                        (/= (plist-get case :from) (plist-get case :to)))
             (error "T3 move operation needs distinct positive indices"))))
        (when (and (memq action '(:delete-row :delete-column
                                 :move-row :move-column))
                   (plist-member case :values))
          (error "T3 delete and move operations do not accept values"))))
    cases))

(defun org-fixture--table-edit-context (table)
  (let ((row-count 0)
        (width 0)
        (section 0)
        (sections nil))
    (dolist (row table)
      (if (eq row 'hline)
          (setq section (1+ section))
        (progn
          (setq row-count (1+ row-count)
                width (max width (length row)))
          (push (cons row-count section) sections))))
    (list :row-count row-count
          :width width
          :sections (nreverse sections))))

(defun org-fixture--table-edit-row-section (context row)
  (cdr (assq row (plist-get context :sections))))

(defun org-fixture--table-edit-set-values (table-start first-row column values)
  (let ((row first-row))
    (dolist (value values)
      (goto-char table-start)
      (org-table-put row column value)
      (setq row (1+ row)))
    (org-table-align)))

(defun org-fixture--table-edit-set-row-values (table-start row values)
  (let ((column 1))
    (dolist (value values)
      (goto-char table-start)
      (org-table-put row column value)
      (setq column (1+ column)))
    (org-table-align)))

(defun org-fixture--table-edit-run (case)
  (let* ((source (plist-get case :source))
         (action (plist-get case :action))
         (index (plist-get case :index))
         (from (plist-get case :from))
         (to (plist-get case :to))
         (values (plist-get case :values)))
    (with-temp-buffer
      (org-fixture--read-buffer source)
      (let ((before (buffer-string))
            table-start table context)
        (goto-char (point-min))
        (unless (re-search-forward "^[ \t]*|" nil t)
          (error "T3 source has no pipe table"))
        (setq table-start (line-beginning-position))
        (unless (= (length (org-element-map (org-element-parse-buffer)
                                            'table #'identity))
                   1)
          (error "T3 source must contain exactly one table"))
        (goto-char table-start)
        (setq table (org-table-to-lisp)
              context (org-fixture--table-edit-context table))
        (dolist (metadata org-fixture--table-edit-required-metadata)
          (unless (string-match-p (regexp-quote metadata) before)
            (error "Missing T3 fixture metadata %S" metadata)))
        (pcase action
          (:insert-row
           (unless (<= index (1+ (plist-get context :row-count)))
             (error "T3 row insertion index is outside the table"))
           (when (> (length values) (plist-get context :width))
             (error "T3 row insertion values exceed the table width"))
           (goto-char table-start)
           (if (= index (1+ (plist-get context :row-count)))
               (progn
                 (org-table-goto-line (plist-get context :row-count))
                 (org-table-insert-row t))
             (org-table-goto-line index)
             (org-table-insert-row))
           (org-fixture--table-edit-set-row-values table-start index values))
          (:delete-row
           (unless (<= index (plist-get context :row-count))
             (error "T3 row deletion index is outside the table"))
           (goto-char table-start)
           (org-table-goto-line index)
           (org-table-kill-row))
          (:move-row
           (unless (and (<= from (plist-get context :row-count))
                        (<= to (plist-get context :row-count)))
             (error "T3 row move index is outside the table"))
           (unless (equal (org-fixture--table-edit-row-section context from)
                          (org-fixture--table-edit-row-section context to))
             (error "T3 oracle row move crosses an hline boundary"))
           (let ((row from))
             (while (/= row to)
               (goto-char table-start)
               (org-table-goto-line row)
               (org-table-move-row (> row to))
               (setq row (+ row (if (< row to) 1 -1))))))
          (:insert-column
           (unless (<= index (1+ (plist-get context :width)))
             (error "T3 column insertion index is outside the table"))
           (when (> (length values) (plist-get context :row-count))
             (error "T3 column insertion values exceed the row count"))
           (goto-char table-start)
           (org-table-goto-column index)
           (org-table-insert-column)
           (org-fixture--table-edit-set-values table-start 1 index values))
          (:delete-column
           (unless (<= index (plist-get context :width))
             (error "T3 column deletion index is outside the table"))
           (goto-char table-start)
           (org-table-goto-column index)
           (org-table-delete-column))
          (:move-column
           (unless (and (<= from (plist-get context :width))
                        (<= to (plist-get context :width)))
             (error "T3 column move index is outside the table"))
           (let ((column from))
             (while (/= column to)
               (goto-char table-start)
               (org-table-goto-column column)
               (org-table-move-column (> column to))
               (setq column (+ column (if (< column to) 1 -1)))))))
        (list before (buffer-string))))))

(defun org-fixture--table-edit-diff (before after source-label result-label)
  (let ((old-buffer (generate-new-buffer "t3-source"))
        (new-buffer (generate-new-buffer "t3-source-after"))
        diff-buffer)
    (unwind-protect
        (progn
          (with-current-buffer old-buffer (insert before))
          (with-current-buffer new-buffer (insert after))
          (setq diff-buffer (diff-no-select old-buffer new-buffer "-u" t))
          (with-current-buffer diff-buffer
            (let* ((raw (buffer-string))
                   (start (string-match "^@@ " raw))
                   (body (and start (substring raw start)))
                   (end (and body (string-match "^Diff finished\\." body))))
              (unless body
                (error "T3 source-after has no diff"))
              (replace-regexp-in-string
               "\n+\\'" "\n"
               (concat "--- " source-label "\n+++ " result-label "\n"
                       (if end (substring body 0 end) body))))))
      (when (buffer-live-p diff-buffer) (kill-buffer diff-buffer))
      (kill-buffer old-buffer)
      (kill-buffer new-buffer))))

(defun org-fixture--table-edit-trailing-space-p (text)
  (string-match-p "[ \t]+\n" text))

(defun org-fixture--table-edit-lines (text)
  (unless (and (> (length text) 0)
               (= (aref text (1- (length text))) ?\n))
    (error "T3 oracle text must end with a newline"))
  (mapcar #'substring-no-properties
          (butlast (split-string text "\n" nil))))

(defun org-fixture--table-edit-join-lines (lines)
  (concat (mapconcat #'identity lines "\n") "\n"))

(defun org-fixture--table-edit-encoded-oracle (source-after diff-content)
  (let* ((print-length nil)
         (print-level nil)
         (encoded
          (with-temp-buffer
            (prin1 (list :source-after
                         (org-fixture--table-edit-lines source-after)
                         :diff
                         (org-fixture--table-edit-lines diff-content))
                   (current-buffer))
            (insert "\n")
            (buffer-string)))
         (decoded
          (let ((read-eval nil))
            (car (read-from-string encoded)))))
    (unless (and (equal (substring-no-properties source-after)
                        (org-fixture--table-edit-join-lines
                         (plist-get decoded :source-after)))
                 (equal (substring-no-properties diff-content)
                        (org-fixture--table-edit-join-lines
                         (plist-get decoded :diff))))
      (error "T3 encoded oracle did not round-trip exact Org bytes"))
    encoded))

(defun org-fixture-generate-table-edit (corpus-directory operations-file
                                                        expected-directory)
  "Generate Org source-after and unified-diff oracles for T3 operations."
  (unless (and (equal emacs-version "31.1")
               (equal (org-version) "9.8.10"))
    (error "T3 oracle requires Emacs 31.1 and Org 9.8.10; found %s and %s"
           emacs-version (org-version)))
  (unless (file-directory-p corpus-directory)
    (error "T3 corpus directory does not exist"))
  (let ((cases (org-fixture--table-edit-validate-cases
                (org-fixture--table-edit-read-cases operations-file))))
    (dolist (case cases)
      (let* ((id (plist-get case :id))
             (source (plist-get case :source))
             (input (expand-file-name source corpus-directory))
             (source-after-directory
              (expand-file-name "source-after" expected-directory))
             (diff-directory (expand-file-name "diff" expected-directory))
             (source-after (expand-file-name (concat id ".org")
                                             source-after-directory))
             (diff-file (expand-file-name (concat id ".diff") diff-directory))
             (encoded-directory (expand-file-name "encoded" expected-directory))
             (encoded-file (expand-file-name (concat id ".sexp")
                                             encoded-directory))
             encode-p before after diff-content)
        (unless (file-exists-p input)
          (error "T3 corpus source does not exist: %s" source))
        (pcase-let ((`(,old ,new) (org-fixture--table-edit-run
                                   (let ((case (copy-sequence case)))
                                     (plist-put case :source input)
                                     case))))
          (setq before old
                after new))
        (setq diff-content
              (org-fixture--table-edit-diff
               before after (concat "corpus/" source)
               (concat "source-after/" id ".org")))
        (setq encode-p
              (or (org-fixture--table-edit-trailing-space-p after)
                  (org-fixture--table-edit-trailing-space-p diff-content)))
        (make-directory source-after-directory t)
        (make-directory diff-directory t)
        (let ((coding-system-for-write 'utf-8-unix))
          (if encode-p
              (progn
                (make-directory encoded-directory t)
                (when (file-exists-p source-after) (delete-file source-after))
                (when (file-exists-p diff-file) (delete-file diff-file))
                (write-region
                 (org-fixture--check-output
                  (org-fixture--table-edit-encoded-oracle after diff-content))
                 nil encoded-file nil 'silent))
            (progn
              (when (file-exists-p encoded-file) (delete-file encoded-file))
              (write-region (org-fixture--check-output after)
                            nil source-after nil 'silent)
              (write-region (org-fixture--check-output diff-content)
                            nil diff-file nil 'silent))))
        (princ (format "Wrote T3 oracle %s\n" id))))
    (princ (format "Generated %d T3 table-edit oracle cases.\n" (length cases)))
    (princ (format "Emacs %s; Org %s\n" emacs-version (org-version)))))

(defun org-fixture--table-edit-dispatch-around (original)
  (let ((arguments (if (equal (car command-line-args-left) "--")
                       (cdr command-line-args-left)
                     command-line-args-left)))
    (if (equal (car arguments) "--table-edit")
        (progn
          (setq command-line-args-left nil)
          (unless (= (length arguments) 4)
            (error (concat
                    "Usage: emacs --batch -Q --script scripts/generate-org-fixtures.el "
                    "--table-edit CORPUS-DIR OPERATIONS-FILE EXPECTED-DIR")))
          (org-fixture-generate-table-edit
           (expand-file-name (nth 1 arguments))
           (expand-file-name (nth 2 arguments))
           (expand-file-name (nth 3 arguments))))
      (funcall original))))

(advice-add 'org-fixture--dispatch-main :around
            #'org-fixture--table-edit-dispatch-around)


(defun org-fixture--e-pretty-data (data)
  (require 'pp)
  (let ((form (car (read-from-string (org-fixture--cl-value data))))
        (replacements nil)
        (next 0))
    (cl-labels
        ((prepare (value)
           (cond
            ((stringp value)
             (let* ((name (format "org-fixture--e-string-%d-end" next))
                    (marker (make-symbol name)))
               (setq next (1+ next))
               (push (cons marker (substring-no-properties value))
                     replacements)
               marker))
            ((consp value) (cons (prepare (car value)) (prepare (cdr value))))
            ((and (vectorp value) (not (stringp value)))
             (mapcar #'prepare (append value nil)))
            (t value))))
      (let ((printed (pp-to-string (prepare form))))
        (dolist (entry (nreverse replacements) printed)
          (setq printed
                (replace-regexp-in-string
                 (regexp-quote (symbol-name (car entry)))
                 (org-fixture--cl-string (cdr entry))
                 printed t t)))))))

(defun org-fixture--e-write-case (root relative data)
  (let* ((output (expand-file-name relative root))
         (content
          (concat
           (format "; Generated from Org APIs with Emacs %s and Org %s.\n"
                   emacs-version (org-version))
           "; Org oracle output is kept separate from contract policy output.\n"
           (org-fixture--e-pretty-data data))))
    (org-fixture--check-output content)
    (when (string-match-p "\\(?:^\\|[[:space:]]\\)~/" content)
      (error "Generated E fixture unexpectedly contains a home-relative path"))
    (make-directory (file-name-directory output) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region content nil output nil 'silent))
    relative))

(defun org-fixture--e-direct-child-headlines ()
  (let (headlines)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\* \\(.*\\)$" nil t)
        (push (match-string-no-properties 1) headlines)))
    (nreverse headlines)))

(defun org-fixture--e-sort-key-value (key property-name)
  (pcase key
    ((or :alpha :numeric) (org-get-heading t t t t))
    (:time (save-excursion
             (when (re-search-forward org-ts-regexp nil t)
               (match-string-no-properties 0))))
    (:created (org-entry-get nil property-name))
    (:scheduled (org-entry-get nil "SCHEDULED"))
    (:deadline (org-entry-get nil "DEADLINE"))
    (:priority (org-get-priority (org-get-heading)))
    (:todo (org-get-todo-state))
    (:property (org-entry-get nil property-name))))

(defun org-fixture--e-sort-key-values (key property-name)
  (let (values)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\* " nil t)
        (let ((heading-start (match-beginning 0)))
          (goto-char heading-start)
          (push (org-fixture--e-sort-key-value key property-name) values)
          (forward-line 1))))
    (nreverse values)))

(defun org-fixture--e-sort-case (spec)
  (let* ((input (plist-get spec :input))
         (key (plist-get spec :key))
         (property-name (plist-get spec :property-name))
         (sort-character (plist-get spec :sort-character))
         (org-priority-highest ?A)
         (org-priority-lowest ?C)
         (org-priority-default ?B))
    (with-temp-buffer
      (insert input)
      (org-mode)
      (goto-char (point-min))
      (unless (re-search-forward "^\\* Parent$" nil t)
        (error "E sort fixture has no parent headline: %s"
               (plist-get spec :name)))
      (beginning-of-line)
      (org-sort-entries nil sort-character nil nil property-name)
      (append
       (list :name (plist-get spec :name)
             :emacs-version emacs-version
             :org-version (org-version)
             :operation :sort
             :key key
             :org-type :headline
             :sort-character sort-character)
       (when property-name (list :property-name property-name))
       (when (eq key :priority)
         (list :priority-highest (char-to-string org-priority-highest)
               :priority-default (char-to-string org-priority-default)
               :priority-lowest (char-to-string org-priority-lowest)))
       (list :input input
             :expected (buffer-string)
             :expected-order (org-fixture--e-direct-child-headlines)
             :expected-key-values
             (org-fixture--e-sort-key-values key property-name))))))

(defun org-fixture--e-sort-specs ()
  (list
   (list :name "alpha" :key :alpha :sort-character ?a
         :input (concat "* Parent\n"
                        "** beta\n** Alpha\n** alpha\n** alpha\n** Gamma\n"))
   (list :name "numeric" :key :numeric :sort-character ?n
         :input (concat "* Parent\n"
                        "** 10\n** 2\n** 2.0\n** 1\n"))
   (list :name "time" :key :time :sort-character ?t
         :input (concat "* Parent\n"
                        "** Later <2024-03-01 Fri>\n"
                        "** Early <2024-02-01 Thu>\n"
                        "** Tie first <2024-02-01 Thu>\n"
                        "** No timestamp\n"))
   (list :name "created" :key :created :sort-character ?r
         :property-name "CREATED"
         :input (concat "* Parent\n"
                        "** Later\n:PROPERTIES:\n:CREATED: [2024-03-01 Fri]\n:END:\n"
                        "** Early\n:PROPERTIES:\n:CREATED: [2024-02-01 Thu]\n:END:\n"
                        "** Tie first\n:PROPERTIES:\n:CREATED: [2024-02-01 Thu]\n:END:\n"))
   (list :name "scheduled" :key :scheduled :sort-character ?s
         :input (concat "* Parent\n"
                        "** Later\nSCHEDULED: <2024-03-01 Fri>\n"
                        "** Early\nSCHEDULED: <2024-02-01 Thu>\n"
                        "** Tie first\nSCHEDULED: <2024-02-01 Thu>\n"
                        "** Unscheduled\n"))
   (list :name "deadline" :key :deadline :sort-character ?d
         :input (concat "* Parent\n"
                        "** Later\nDEADLINE: <2024-03-01 Fri>\n"
                        "** Early\nDEADLINE: <2024-02-01 Thu>\n"
                        "** Tie first\nDEADLINE: <2024-02-01 Thu>\n"
                        "** No deadline\n"))
   (list :name "priority" :key :priority :sort-character ?p
         :input (concat "* Parent\n"
                        "** [#B] Explicit B\n** Default priority\n"
                        "** [#A] High\n** [#C] Low\n** [#B] Second B\n"))
   (list :name "todo" :key :todo :sort-character ?o
         :input (concat "#+TODO: TODO WAIT | DONE\n"
                        "* Parent\n** WAIT Waiting\n** DONE Finished\n"
                        "** TODO Active\n"))
   (list :name "property" :key :property :sort-character ?r
         :property-name "RANK"
         :input (concat "* Parent\n"
                        "** Zulu\n:PROPERTIES:\n:RANK: z\n:END:\n"
                        "** Alpha first\n:PROPERTIES:\n:RANK: a\n:END:\n"
                        "** Alpha second\n:PROPERTIES:\n:RANK: a\n:END:\n"
                        ))
   (list :name "priority-directive" :key :priority :sort-character ?p
         :input (concat "#+PRIORITIES: B D C\n"
                        "* Parent\n** [#C] Explicit default\n"
                        "** [#D] Lowest\n** Default priority\n"
                        "** [#B] Highest\n"))))

(defun org-fixture--e-refile-case (name source-input target-input target-heading)
  (let* ((source-buffer (generate-new-buffer " *e-refile-source*"))
         (target-buffer (generate-new-buffer " *e-refile-target*"))
         (root (file-name-directory
                (directory-file-name (file-name-directory load-file-name))))
         (source-file (expand-file-name "e-refile-source.org" root))
         (target-file (expand-file-name "e-refile-target.org" root))
         (target-marker nil)
         (target-label (or target-heading "Root")))
    (unwind-protect
        (progn
          (with-current-buffer target-buffer
            (insert target-input)
            (org-mode)
            (setq buffer-file-name target-file)
            (if target-heading
                (progn
                  (goto-char (point-min))
                  (unless (re-search-forward
                           (concat "^\\*+ " (regexp-quote target-heading) "$"))
                    (error "Missing E refile target heading: %s" target-heading))
                  (beginning-of-line))
              (goto-char (point-min)))
            (setq target-marker (copy-marker (point) t)))
          (with-current-buffer source-buffer
            (insert source-input)
            (org-mode)
            (setq buffer-file-name source-file)
            (let ((org-refile-keep nil)
                  (org-refile-use-outline-path nil)
                  (org-reverse-note-order nil)
                  (org-refile-allow-creating-parent-nodes t)
                  (inhibit-message t))
              (org-refile nil nil
                          (list target-label target-file nil target-marker)))
            (let ((source-expected (buffer-string))
                  target-expected)
              (with-current-buffer target-buffer
                (setq target-expected (buffer-string)))
              (list :name name
                    :emacs-version emacs-version
                    :org-version (org-version)
                    :operation :refile
                    :mode :move
                    :target-heading target-heading
                    :source-input source-input
                    :target-input target-input
                    :source-expected source-expected
                    :target-expected target-expected))))
      (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
      (when (buffer-live-p target-buffer) (kill-buffer target-buffer)))))

(defun org-fixture--e-archive-case (name source-name source-input location)
  (require 'org-archive)
  (let* ((root (file-name-directory
                (directory-file-name (file-name-directory load-file-name))))
         (source-file (expand-file-name source-name root))
         (source-buffer (generate-new-buffer " *e-archive-source*"))
         (archive-buffer nil)
         (fixed-time (encode-time 0 34 12 19 2 2024 "UTC"))
         (fixed-format (org-time-stamp-format 'with-time 'no-brackets))
         (fixed-time-string nil)
         (original-format-time-string (symbol-function 'format-time-string)))
    (unwind-protect
        (with-current-buffer source-buffer
          (insert source-input)
          (org-mode)
          (setq buffer-file-name source-file)
          (setq fixed-time-string
                (funcall original-format-time-string fixed-format fixed-time "UTC"))
          (let* ((org-archive-location location)
                 (org-archive-subtree-save-file-p nil)
                 (org-archive-save-context-info '(time file olpath category todo itags))
                 (org-archive-subtree-add-inherited-tags 'infile)
                 (org-archive-mark-done nil)
                 (location-pair (org-archive--compute-location location))
                 (archive-file (car location-pair)))
            (cl-letf (((symbol-function 'format-time-string)
                       (lambda (format-string &optional time zone)
                         (if (and (null time) (equal format-string fixed-format))
                             (funcall original-format-time-string
                                      format-string fixed-time "UTC")
                           (funcall original-format-time-string
                                    format-string time zone)))))
              (goto-char (point-min))
              (unless (re-search-forward "^\\*+ \\(?:TODO \\)?Work\\b" nil t)
                (error "Missing E archive source heading: %s" name))
              (beginning-of-line)
              (let ((inhibit-message t)) (org-archive-subtree)))
            (setq archive-buffer
                  (or (get-file-buffer archive-file)
                      (error "Org archive buffer missing for %s" name)))
            (with-current-buffer archive-buffer
              (list :name name
                    :emacs-version emacs-version
                    :org-version (org-version)
                    :operation :archive
                    :archive-location location
                    :archive-time fixed-time-string
                    :source-file source-name
                    :source-input source-input
                    :target-input ""
                    :source-expected
                    (with-current-buffer source-buffer (buffer-string))
                    :target-expected (buffer-string)))))
      (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
      (when (and (buffer-live-p archive-buffer)
                 (not (eq archive-buffer source-buffer)))
        (kill-buffer archive-buffer)))))

(defun org-fixture--e-normalize-archive-data (data source-name)
  (let* ((root (file-name-directory
                (directory-file-name (file-name-directory load-file-name))))
         (source-file (expand-file-name source-name root))
         (source-abbreviated (abbreviate-file-name source-file))
         (archive-file (concat source-file "_archive"))
         (archive-abbreviated (abbreviate-file-name archive-file)))
    (mapcar (lambda (value)
              (if (stringp value)
                  (let* ((normalized
                         (replace-regexp-in-string
                          (regexp-quote source-file) source-name value t t))
                         (normalized
                          (replace-regexp-in-string
                           (regexp-quote source-abbreviated) source-name
                           normalized t t)))
                    (let ((normalized
                           (replace-regexp-in-string
                            (regexp-quote archive-file)
                            (concat source-name "_archive") normalized t t)))
                      (replace-regexp-in-string
                       (regexp-quote archive-abbreviated)
                       (concat source-name "_archive") normalized t t)))
                value))
            data)))

(defun org-fixture--e-without-archive-time (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "^:ARCHIVE_TIME:.*$" nil t)
      (beginning-of-line)
      (delete-region (point) (line-end-position))
      (when (eq (char-after) ?\n) (delete-char 1)))
    (buffer-string)))

(defun org-fixture--e-policy-archive (oracle-data)
  (let* ((source-name (plist-get oracle-data :source-file))
         (source-file (plist-get oracle-data :source-expected))
         (org-target (plist-get oracle-data :target-expected))
         (headline-start (or (string-match "^\\*+ \\(?:TODO \\)?Work\\b" org-target)
                             (error "Missing root archive headline")))
         (target (substring org-target headline-start))
         (without-time (org-fixture--e-without-archive-time target)))
    (list :name "archive-time-omitted"
          :emacs-version emacs-version
          :org-version (org-version)
          :operation :archive
          :source-file source-name
          :divergence :archive-time-omitted-no-time-context
          :additional-divergence
          :org-archive-file-header-omitted-for-caller-target
          :oracle-archive-time (plist-get oracle-data :archive-time)
          :source-input (plist-get oracle-data :source-input)
          :target-input ""
          :source-expected source-file
          :target-expected without-time
          :oracle-target-expected org-target)))

(defun org-fixture--e-generate (output-directory)
  (unless (and (equal emacs-version "31.1")
               (equal (org-version) "9.8.10"))
    (error "E oracle requires Emacs 31.1 and Org 9.8.10; found %s / %s"
           emacs-version (org-version)))
  (let* ((root (file-name-directory
                (directory-file-name (file-name-directory load-file-name))))
         (expected-oracle (expand-file-name "t/fixtures/e/oracle" root))
         (expected-policy (expand-file-name "t/fixtures/e/policy" root))
         (output-directory (expand-file-name output-directory))
         (policy-directory (expand-file-name "../policy" output-directory))
         (written nil)
         (manifest nil))
    (unless (equal output-directory expected-oracle)
      (error "E oracle output must be t/fixtures/e/oracle"))
    (unless (equal policy-directory expected-policy)
      (error "E policy output must be t/fixtures/e/policy"))
    (dolist (spec (org-fixture--e-sort-specs))
      (let* ((data (org-fixture--e-sort-case spec))
             (relative (concat "sort/" (plist-get spec :name) ".sexp")))
        (push (org-fixture--e-write-case output-directory relative data) written)
        (push (list :name (plist-get spec :name) :path relative
                    :key (plist-get spec :key)) manifest)))
    (dolist (spec
             (list
              (list "refile-root"
                    "* Source\nsource body\n" "" nil)
              (list "refile-heading"
                    "* Parent\n** Source\nsource body\n"
                    "* Target\ntarget body\n" "Target")
              (list "refile-incompatible-todo"
                    "#+TODO: WAIT | CLOSED\n* WAIT Source\nsource body\n"
                    "#+TODO: TODO | DONE\n* Target\ntarget body\n" "Target")))
      (let* ((name (nth 0 spec))
             (data (org-fixture--e-refile-case
                    name (nth 1 spec) (nth 2 spec) (nth 3 spec)))
             (relative (concat "relocate/" name ".sexp")))
        (push (org-fixture--e-write-case output-directory relative data) written)
        (push (list :name name :path relative :key :refile) manifest)))
    (let ((archive-specs
           (progn
             (require 'org-archive)
             (list
              (list "archive-root" "e-oracle-root-source.org"
                    "* Keep\nkeep body\n* Work\nwork body\n"
                    (default-value 'org-archive-location) :default)
              (list "archive-heading" "e-oracle-heading-source.org"
                    "* Keep\nkeep body\n* Work\nwork body\n"
                    "%s_archive::* Archive")
              (list "archive-context" "e-oracle-context-source.org"
                    "#+FILETAGS: FILE\n* Parent :PARENT:\n** Child :CHILD:\n*** TODO Work :LOCAL:\nBody\n"
                    "%s_archive::")))))
      (dolist (spec archive-specs)
        (let* ((name (nth 0 spec))
               (source-name (nth 1 spec))
               (raw (org-fixture--e-archive-case
                     name source-name (nth 2 spec) (nth 3 spec)))
               (data (org-fixture--e-normalize-archive-data raw source-name))
               (relative (concat "archive/" name ".sexp")))
          (when (eq (nth 4 spec) :default)
            (setq data (plist-put data :archive-location-source :default)))
          (push (org-fixture--e-write-case output-directory relative data) written)
          (push (list :name name :path relative :key :archive) manifest)
          (when (equal name "archive-context")
            (let ((policy (org-fixture--e-policy-archive data)))
              (org-fixture--e-write-case
               policy-directory "archive/archive-time-omitted.sexp" policy)
              (push "../policy/archive/archive-time-omitted.sexp" written)
              (push (list :name "archive-time-omitted"
                          :path "../policy/archive/archive-time-omitted.sexp"
                          :key :archive :policy t)
                    manifest))))))
    (dolist (spec
             (list
              (list :name "property-missing-last" :key :property
                    :sort-character ?r :property-name "RANK"
                    :divergence :org-property-missing-sorts-first
                    :input (concat "* Parent\n** Zulu\n:PROPERTIES:\n:RANK: z\n:END:\n"
                                   "** Missing property\n** Alpha\n:PROPERTIES:\n:RANK: a\n:END:\n"))
              (list :name "created-missing-last" :key :created
                    :sort-character ?r :property-name "CREATED"
                    :divergence :org-created-missing-sorts-first
                    :input (concat "* Parent\n"
                                   "** Later\n:PROPERTIES:\n:CREATED: [2024-03-01 Fri]\n:END:\n"
                                   "** Missing\n"
                                   "** Early\n:PROPERTIES:\n:CREATED: [2024-02-01 Thu]\n:END:\n"
                                   "** Tie first\n:PROPERTIES:\n:CREATED: [2024-02-01 Thu]\n:END:\n"))
              (list :name "todo-missing-last" :key :todo :sort-character ?o
                    :divergence :org-todo-missing-is-not-last
                    :input (concat "#+TODO: TODO WAIT | DONE\n* Parent\n"
                                   "** DONE Finished\n** No TODO state\n"
                                   "** WAIT Waiting\n** TODO Active\n"))
              (list :name "alpha-code-point-case-sensitive" :key :alpha
                    :sort-character ?a
                    :divergence :org-alpha-is-case-insensitive
                    :input "* Parent\n** a\n** A\n")))
      (let* ((key (plist-get spec :key))
             (property-name (plist-get spec :property-name))
             (oracle-data (org-fixture--e-sort-case spec))
             (expected-order
              (pcase key
                (:property '("Alpha" "Zulu" "Missing property"))
                (:created '("Early" "Tie first" "Later" "Missing"))
                (:todo '("TODO Active" "WAIT Waiting" "DONE Finished"
                         "No TODO state"))
                (:alpha '("A" "a"))))
             (expected-key-values
              (pcase key
                (:property '("a" "z" nil))
                (:created '("[2024-02-01 Thu]" "[2024-02-01 Thu]"
                            "[2024-03-01 Fri]" nil))
                (:todo '("TODO" "WAIT" "DONE" nil))
                (:alpha '("A" "a"))))
             (data (list :name (plist-get spec :name)
                         :emacs-version emacs-version
                         :org-version (org-version)
                         :operation :sort
                         :key key
                         :property-name property-name
                         :divergence (plist-get spec :divergence)
                         :input (plist-get spec :input)
                         :expected-order expected-order
                         :expected-key-values expected-key-values
                         :oracle-expected-order
                         (plist-get oracle-data :expected-order)
                         :oracle-key-values
                         (plist-get oracle-data :expected-key-values))))
        (org-fixture--e-write-case
         policy-directory (concat "sort/" (plist-get spec :name) ".sexp") data)
        (push (concat "../policy/sort/" (plist-get spec :name) ".sexp")
              written)
        (push (list :name (plist-get spec :name)
                    :path (concat "../policy/sort/"
                                  (plist-get spec :name) ".sexp")
                    :key key :policy t)
              manifest)))
    (let ((manifest-data
           (list :name "e-oracle-manifest"
                 :emacs-version emacs-version
                 :org-version (org-version)
                 :cases (nreverse manifest))))
      (push (org-fixture--e-write-case output-directory "manifest.sexp"
                                      manifest-data)
            written))
    (let ((generated-count (length written)))
      (dolist (relative (nreverse written))
        (princ (format "Wrote %s\n" relative)))
      (princ (format "Generated %d E oracle and policy fixtures.\n"
                     generated-count)))
    (princ (format "Emacs %s; Org %s\n" emacs-version (org-version)))))

(defun org-fixture--e-main-around (original)
  (let ((arguments (if (equal (car command-line-args-left) "--")
                       (cdr command-line-args-left)
                     command-line-args-left)))
    (if (equal (car arguments) "--e-oracle")
        (progn
          (setq command-line-args-left nil)
          (unless (= (length arguments) 2)
            (error (concat
                    "Usage: emacs --batch -Q --script scripts/generate-org-fixtures.el "
                    "--e-oracle t/fixtures/e/oracle")))
          (org-fixture--e-generate (nth 1 arguments)))
      (funcall original))))

(advice-add 'org-fixture--dispatch-main :around #'org-fixture--e-main-around)

(defun org-fixture--html-export-without-native-source-fontification
    (original source-file backend)
  (if (eq backend 'html)
      (let ((org-src-fontify-natively nil)
            (org-html-htmlize-output-type nil))
        (funcall original source-file backend))
    (funcall original source-file backend)))

(advice-add 'org-fixture--export-buffer
            :around #'org-fixture--html-export-without-native-source-fontification)

(defun org-fixture--html-export-preserves-source-modes-without-evaluation
    (original backend &rest arguments)
  (if (eq backend 'html)
      (let ((org-export-use-babel t))
        (require 'ob-core)
        (cl-letf (((symbol-function 'org-babel-check-confirm-evaluate)
                   (lambda (&rest _arguments) nil)))
          (apply original backend arguments)))
    (apply original backend arguments)))

(advice-add 'org-export-as :around
            #'org-fixture--html-export-preserves-source-modes-without-evaluation)

(defun org-fixture--html-export-metadata (original backend source include
                                                  source-sha include-sha)
  (let ((metadata (funcall original backend source include source-sha include-sha)))
    (if (eq backend 'html)
        (concat metadata "html-source-fontification=disabled\n"
                "html-htmlize-output-type=disabled\n"
                "html-babel-evaluation=disabled\n")
      metadata)))

(advice-add 'org-fixture--export-metadata
            :around #'org-fixture--html-export-metadata)

(defun org-fixture-generate-html-oracle
    (corpus-directory manifest expected-directory)
  "Generate HTML export outputs for the literal HTML-only oracle manifest."
  (unless (file-directory-p corpus-directory)
    (error "HTML export oracle corpus directory does not exist"))
  (require 'ox-html)
  (require 'ox-ascii)
  (org-fixture--verify-ox-gfm-pin)
  (let* ((cases (org-fixture--read-export-manifest manifest corpus-directory))
         (temporary-expected (make-temp-file "org-fixture-html-oracle-" t))
         (org-export-use-babel nil))
    (unwind-protect
        (progn
          (dolist (case cases)
            (let* ((case-name (nth 0 case))
                   (temporary-html (expand-file-name
                                    "html" temporary-expected))
                   (expected-html (expand-file-name "html"
                                                     expected-directory)))
              (org-fixture--generate-export-case
               case corpus-directory temporary-expected)
              (make-directory expected-html t)
              (dolist (extension '("html" "meta"))
                (copy-file
                 (expand-file-name (format "%s.%s" case-name extension)
                                   temporary-html)
                 (expand-file-name (format "%s.%s" case-name extension)
                                   expected-html)
                 t))))
          (princ (format "Generated %d HTML export oracle cases.\n"
                         (length cases)))
          (princ (format "Emacs %s; Org %s\n" emacs-version (org-version))))
      (delete-directory temporary-expected t))))

(defun org-fixture--html-oracle-dispatch (original)
  (if (equal (car command-line-args-left) "--html-oracle")
      (let ((arguments command-line-args-left))
        (setq command-line-args-left nil)
        (unless (= (length arguments) 4)
          (error (concat
                  "Usage: emacs --batch -Q --script scripts/generate-org-fixtures.el "
                  "--html-oracle CORPUS-DIR MANIFEST EXPECTED-DIR")))
        (org-fixture-generate-html-oracle
         (expand-file-name (nth 1 arguments))
         (expand-file-name (nth 2 arguments))
         (expand-file-name (nth 3 arguments))))
    (funcall original)))

(advice-add 'org-fixture--dispatch-main :around
            #'org-fixture--html-oracle-dispatch)

(org-fixture--dispatch-main)

;;; generate-org-fixtures.el ends here

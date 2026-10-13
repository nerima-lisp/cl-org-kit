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

(org-fixture--dispatch-main)

;;; generate-org-fixtures.el ends here

;;;; src/org-semantics.lisp
(in-package #:org-kit)

(defparameter +org-setting-keys+
  '(("CATEGORY" . :category)
    ("FILETAGS" . :file-tags)
    ("INLINETASK-MIN-LEVEL" . :inlinetask-min-level)
    ("LINK" . :link-abbrevs)
    ("LINK-ABBREVS" . :link-abbrevs)
    ("ODD-LEVELS-ONLY-P" . :odd-levels-only-p)
    ("STARTUP" . :startup)
    ("TAG-ALIST" . :tag-alist)
    ("TAGS" . :tag-alist)
    ("TODO-KEYWORDS" . :todo-keywords)))

(defun org-setting-key (key)
  "Return the document-settings key for an Org setting name, or NIL if unknown.
KEY may be a string or symbol; names are case-insensitive."
  (let* ((source-name (if (stringp key) key (symbol-name key)))
         (name (string-upcase source-name)))
    (cond
      ((member name '("TODO" "SEQ_TODO" "TYP_TODO") :test #'string=)
       :todo-keywords)
      ((and (>= (length name) 6)
            (or (string= name "TODO[" :end1 5 :end2 5)
                (and (>= (length name) 9)
                     (string= name "SEQ_TODO[" :end1 9 :end2 9))
                (and (>= (length name) 9)
                     (string= name "TYP_TODO[" :end1 9 :end2 9)))
            (char= (char name (1- (length name))) #\]))
       :todo-keywords)
      (t (cdr (assoc name +org-setting-keys+ :test #'string=))))))

(defun org-startup-odd-levels-p (startup)
  "Return whether the last recognized odd-level option in STARTUP is ODD."
  (loop with odd-levels-p = nil
        for option in startup
        do (cond
             ((string-equal option "odd")
              (setf odd-levels-p t))
             ((string-equal option "oddeven")
              (setf odd-levels-p nil)))
        finally (return odd-levels-p)))

(defun org-setting-value (document key &optional default)
  "Return DOCUMENT's setting named by KEY, or DEFAULT when KEY is unknown.
The derived :ODD-LEVELS-ONLY-P setting honors an explicit override, then the
last ODD or ODDEVEN token in the document's STARTUP setting."
  (let* ((settings (document-settings document))
         (resolved-key (org-setting-key key)))
    (cond
      ((null resolved-key) default)
      ((eq resolved-key :odd-levels-only-p)
       (if (member :odd-levels-only-p settings :test #'eq)
           (not (null (getf settings :odd-levels-only-p)))
           (org-startup-odd-levels-p (getf settings :startup))))
      (t (getf settings resolved-key default)))))

(defun logical-heading-level (document star-count)
  "Return Org's logical heading level for STAR-COUNT source stars in DOCUMENT.
With :STARTUP odd, source levels map by 1 + floor(STAR-COUNT / 2)."
  (if (org-setting-value document :odd-levels-only-p)
      (1+ (floor star-count 2))
      star-count))

(defun source-heading-level (document logical-level)
  "Return a canonical source star count for LOGICAL-LEVEL in DOCUMENT.
In odd-level mode this returns the odd star count that maps to LOGICAL-LEVEL."
  (if (org-setting-value document :odd-levels-only-p)
      (1- (* 2 logical-level))
      logical-level))

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

(defun org-setting-todo-declaration-prefix (name)
  (loop for (base . kind) in '(("TODO" . :todo)
                               ("SEQ_TODO" . :seq-todo)
                               ("TYP_TODO" . :typ-todo))
        for base-length = (length base)
        when (string= name base)
          return (values kind base-length nil)
        when (and (> (length name) base-length)
                  (string= name base :end1 base-length :end2 base-length)
                  (char= (char name base-length) #\[))
          return (values kind base-length t)
        finally (return (values nil nil nil))))

(defun org-setting-key (key)
  "Return the document-settings key for an Org setting name, or NIL if unknown.
KEY may be a string or symbol; names are case-insensitive."
  (let* ((source-name (if (stringp key) key (symbol-name key)))
         (name (string-upcase source-name)))
    (cond
      ((multiple-value-bind (kind base-length sequence-id-p)
             (org-setting-todo-declaration-prefix name)
         (declare (ignore base-length))
         (and kind
              (or (not sequence-id-p)
                  (char= (char name (1- (length name))) #\]))))
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

;; Mirrors org-startup-options in Org's org.el. Variable names drop the
;; "org-" prefix; symbol values other than T and NIL become keywords.
(defparameter +org-startup-options+
  '(("fold" :startup-folded :fold)
    ("overview" :startup-folded :overview)
    ("nofold" :startup-folded :nofold)
    ("showall" :startup-folded :showall)
    ("show2levels" :startup-folded :show2levels)
    ("show3levels" :startup-folded :show3levels)
    ("show4levels" :startup-folded :show4levels)
    ("show5levels" :startup-folded :show5levels)
    ("showeverything" :startup-folded :showeverything)
    ("content" :startup-folded :content)
    ("indent" :startup-indented t)
    ("noindent" :startup-indented nil)
    ("num" :startup-numerated t)
    ("nonum" :startup-numerated nil)
    ("hidestars" :hide-leading-stars t)
    ("showstars" :hide-leading-stars nil)
    ("odd" :odd-levels-only t)
    ("oddeven" :odd-levels-only nil)
    ("align" :startup-align-all-tables t)
    ("noalign" :startup-align-all-tables nil)
    ("shrink" :startup-shrink-all-tables t)
    ("descriptivelinks" :link-descriptive t)
    ("literallinks" :link-descriptive nil)
    ("inlineimages" :startup-with-link-previews t)
    ("noinlineimages" :startup-with-link-previews nil)
    ("linkpreviews" :startup-with-link-previews t)
    ("nolinkpreviews" :startup-with-link-previews nil)
    ("latexpreview" :startup-with-latex-preview t)
    ("nolatexpreview" :startup-with-latex-preview nil)
    ("customtime" :display-custom-times t)
    ("logdone" :log-done :time)
    ("lognotedone" :log-done :note)
    ("nologdone" :log-done nil)
    ("lognoteclock-out" :log-note-clock-out t)
    ("nolognoteclock-out" :log-note-clock-out nil)
    ("logrepeat" :log-repeat :state)
    ("lognoterepeat" :log-repeat :note)
    ("logdrawer" :log-into-drawer t)
    ("nologdrawer" :log-into-drawer nil)
    ("logstatesreversed" :log-states-order-reversed t)
    ("nologstatesreversed" :log-states-order-reversed nil)
    ("nologrepeat" :log-repeat nil)
    ("logreschedule" :log-reschedule :time)
    ("lognotereschedule" :log-reschedule :note)
    ("nologreschedule" :log-reschedule nil)
    ("logredeadline" :log-redeadline :time)
    ("lognoteredeadline" :log-redeadline :note)
    ("nologredeadline" :log-redeadline nil)
    ("logrefile" :log-refile :time)
    ("lognoterefile" :log-refile :note)
    ("nologrefile" :log-refile nil)
    ("fninline" :footnote-define-inline t)
    ("nofninline" :footnote-define-inline nil)
    ("fnlocal" :footnote-section nil)
    ("fnauto" :footnote-auto-label t)
    ("fnprompt" :footnote-auto-label nil)
    ("fnconfirm" :footnote-auto-label :confirm)
    ("fnplain" :footnote-auto-label :plain)
    ("fnadjust" :footnote-auto-adjust t)
    ("nofnadjust" :footnote-auto-adjust nil)
    ("fnanon" :footnote-auto-label :anonymous)
    ("constcgs" :constants-unit-system :cgs)
    ("constSI" :constants-unit-system :si)
    ("noptag" :tag-persistent-alist nil)
    ("hideblocks" :hide-block-startup t)
    ("nohideblocks" :hide-block-startup nil)
    ("hidedrawers" :hide-drawer-startup t)
    ("nohidedrawers" :hide-drawer-startup nil)
    ("beamer" :startup-with-beamer-mode t)
    ("entitiespretty" :pretty-entities t)
    ("entitiesplain" :pretty-entities nil)))

(defun org-startup-option (word)
  (assoc word +org-startup-options+ :test #'string-equal))

(defun startup-options (document)
  "Return DOCUMENT's #+STARTUP options as a plist from Org variable to value.
Keys are Org variable names without the org- prefix, such as :LOG-DONE; the
last option naming a variable wins, as in Org. The second value lists the
STARTUP words Org does not recognize, in source order."
  (let ((options nil)
        (unknown nil))
    (dolist (word (getf (document-settings document) :startup))
      (let ((option (org-startup-option word)))
        (if option
            (setf (getf options (second option)) (third option))
            (push word unknown))))
    (values options (nreverse unknown))))

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

(defun effective-semantics-error (target reason message)
  (error 'org-edit-error
         :message message
         :target (bounded-text
                  (if (node-p target)
                      (or (node-property target :raw-value)
                          (string-downcase (symbol-name (node-type target))))
                      (princ-to-string target)))
         :reason reason))

(defun effective-semantics-heading (document heading)
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (node-p heading)
    (effective-semantics-error heading :target-not-found
                               "Heading target was not found"))
  (unless (eq (node-document heading) document)
    (effective-semantics-error heading :foreign-node
                               "Heading does not belong to the input document"))
  (unless (member (node-type heading) '(:headline :inlinetask) :test #'eq)
    (effective-semantics-error heading :wrong-node-type
                               "Target node is not a heading"))
  heading)

(defun effective-query-error (selector position message)
  (error 'org-query-error
         :message message
         :query (bounded-text (prin1-to-string selector))
         :position position))

(defun effective-proper-list-p (value)
  (and (listp value)
       (handler-case (integerp (list-length value))
         (type-error () nil))))

(defun effective-selector-name (name selector)
  (cond
    ((stringp name) name)
    ((symbolp name) (symbol-name name))
    (t (effective-query-error selector 0
                              "Selector entries must be strings or symbols"))))

(defun effective-selector-predicate (selector limits)
  (labels ((compile-selector (spec nested-p)
             (cond
               ((null spec) (constantly nil))
               ((eq spec t) (constantly t))
               ((and (stringp spec) nested-p)
                (let ((name spec))
                  (lambda (candidate) (string= candidate name))))
               ((and (symbolp spec) nested-p)
                (let ((name (symbol-name spec)))
                  (lambda (candidate) (string= candidate name))))
               ((and (consp spec) (eq (first spec) :regexp))
                (unless (and (effective-proper-list-p spec)
                             (= (length spec) 2)
                             (stringp (second spec)))
                  (effective-query-error selector 0
                                         "Regexp selector must contain one string"))
                (let* ((pattern (second spec))
                       (max-pattern-length
                         (limit-value limits :max-input-length))
                       (max-depth (limit-value limits :max-element-depth)))
                  (check-limit limits :max-input-length (length pattern))
                  (let ((regex
                          (handler-case
                              (cl-regex-kit:compile-regex
                               pattern
                               :size-limit (max 1 max-pattern-length)
                               :nest-limit max-depth)
                            (cl-regex-kit:regex-syntax-error (condition)
                              (effective-query-error
                               selector
                               (cl-regex-kit:regex-syntax-error-position condition)
                               "Invalid regexp selector")))))
                    (lambda (candidate)
                      (cl-regex-kit:is-match-p regex candidate)))))
               ((and (consp spec) (eq (first spec) :exclude))
                (unless (effective-proper-list-p spec)
                  (effective-query-error selector 0
                                         "Exclude selector must be a proper list"))
                (let ((predicates
                        (mapcar (lambda (entry)
                                  (compile-selector entry t))
                                (rest spec))))
                  (lambda (candidate)
                    (not (some (lambda (predicate)
                                 (funcall predicate candidate))
                               predicates)))))
               ((effective-proper-list-p spec)
                (let ((names (mapcar (lambda (entry)
                                       (effective-selector-name entry selector))
                                     spec)))
                  (lambda (candidate) (member candidate names :test #'string=))))
               (t
                (effective-query-error selector 0
                                       "Unsupported inheritance selector")))))
    (unless (or (null selector) (eq selector t)
                (effective-proper-list-p selector))
      (effective-query-error selector 0 "Unsupported inheritance selector"))
    (compile-selector selector nil)))

(defun effective-heading-ancestors (heading)
  (loop for ancestor = (node-parent heading) then (node-parent ancestor)
        while ancestor
        when (member (node-type ancestor) '(:headline :inlinetask) :test #'eq)
          collect ancestor))

(defun effective-tag-add (tags tag)
  (append (remove tag tags :test #'string=) (list tag)))

(defun effective-heading-tags (document heading &key (inherit t))
  "Return unique effective tags for HEADING, using INHERIT to select ancestors.
The nearest occurrence of a tag determines its position in the result."
  (let* ((heading (effective-semantics-heading document heading))
         (matches-p (effective-selector-predicate
                     inherit (document-limits document)))
         (ancestors (effective-heading-ancestors heading))
         (tags nil))
    (when inherit
      (dolist (tag (getf (document-settings document) :file-tags))
        (when (funcall matches-p tag)
          (setf tags (effective-tag-add tags tag))))
      (dolist (ancestor (reverse ancestors))
        (dolist (tag (node-property ancestor :tags))
          (when (funcall matches-p tag)
            (setf tags (effective-tag-add tags tag))))))
    (dolist (tag (node-property heading :tags))
      (setf tags (effective-tag-add tags tag)))
    tags))

(defun effective-property-name (name)
  (string-upcase
   (typecase name
     (string name)
     (symbol (symbol-name name))
     (t (error 'type-error :datum name :expected-type '(or string symbol))))))

(defun effective-heading-property-entries (heading key)
  (loop for child in (node-children heading)
        append (loop for drawer in
                       (if (eq (node-type child) :section)
                           (remove-if-not
                            (lambda (node) (eq (node-type node) :property-drawer))
                            (node-children child))
                           (if (eq (node-type child) :property-drawer)
                               (list child)
                               nil))
                     append (loop for property in (node-children drawer)
                                  when (and (eq (node-type property) :node-property)
                                            (stringp (node-property property :key))
                                            (string-equal
                                             (node-property property :key) key))
                                    collect property))))

(defun org-todo-unfinished-heading-p (heading)
  (and (eq (node-type heading) :headline)
       (node-property heading :todo-keyword)
       (not (eq (node-property heading :todo-type) :done))))

(defun org-todo-ordered-heading-p (heading)
  (some (lambda (property)
          (let ((value (node-property property :value)))
            (or (null value)
                (not (string-equal value "nil")))))
        (effective-heading-property-entries heading "ORDERED")))

(defun org-todo-ordered-sibling-blocked-p (heading)
  (labels ((ordered-sibling-blocked-p (current parent)
             (and parent
                  (eq (node-type parent) :headline)
                  (org-todo-ordered-heading-p parent)
                  (loop for sibling in (node-children parent)
                        until (eq sibling current)
                        thereis (org-todo-unfinished-heading-p sibling))))
           (ordered-ancestor-blocked-p (child)
             (and child
                  (org-todo-unfinished-heading-p child)
                  (let ((ancestor (node-parent child)))
                    (and ancestor
                         (eq (node-type ancestor) :headline)
                         (or (ordered-sibling-blocked-p child ancestor)
                             (ordered-ancestor-blocked-p ancestor)))))))
    (let ((parent (node-parent heading)))
      (or (ordered-sibling-blocked-p heading parent)
          (ordered-ancestor-blocked-p parent)))))

(defun org-todo-unfinished-child-p (heading)
  (some (lambda (child)
          (or (org-todo-unfinished-heading-p child)
              (org-todo-unfinished-child-p child)))
        (node-children heading)))

(defun org-todo-blocked-p (heading)
  "Return true when unfinished descendants or ORDERED siblings block HEADING."
  (and (eq (node-type heading) :headline)
       (not (effective-heading-property-entries heading "NOBLOCKING"))
       (or (org-todo-unfinished-child-p heading)
           (org-todo-ordered-sibling-blocked-p heading))))

(defun effective-heading-property-value (heading key)
  (let ((entry (first (effective-heading-property-entries heading key))))
    (if entry
        (values (node-property entry :value) t)
        (values nil nil))))

(defun effective-property-global-value (document name)
  (let ((found-p nil)
        (value nil))
    (dolist (entry (getf (document-settings document) :property-settings))
      (when (and (consp entry)
                 (stringp (car entry))
                 (string-equal (car entry) name))
        (setf found-p t
              value (cdr entry))))
    (values value found-p)))

(defun effective-property-hierarchy-value (headings name global-value global-found-p)
  (let ((values (when global-found-p (list global-value)))
        (sources (when global-found-p (list :document)))
        (plus-p nil))
    (dolist (heading headings)
      (multiple-value-bind (value found-p)
          (effective-heading-property-value heading name)
        (when found-p
          (setf values (list value)
                sources (list heading)
                plus-p nil)))
      (multiple-value-bind (value found-p)
          (effective-heading-property-value heading
                                            (concatenate 'string name "+"))
        (when found-p
          (setf values (append values (list value))
                plus-p t)
          (unless (member heading sources :test #'eq)
            (setf sources (append sources (list heading)))))))
    (values values sources plus-p)))

(defun effective-property (document heading name &key (inherit nil))
  "Return HEADING's effective property value and its source.
The source is a heading node, a root-to-local contributor list when PROPERTY+
contributes, :DOCUMENT for a #+PROPERTY fallback, or NIL when absent."
  (let* ((heading (effective-semantics-heading document heading))
         (name (effective-property-name name))
         (matches-p (effective-selector-predicate
                     inherit (document-limits document)))
         (inherit-p (funcall matches-p name))
         (headings (append
                    (when inherit-p
                      (reverse (effective-heading-ancestors heading)))
                    (list heading)))
         (global-value nil)
         (global-found-p nil))
    (when inherit-p
      (multiple-value-setq (global-value global-found-p)
        (effective-property-global-value document name)))
    (multiple-value-bind (values sources plus-p)
        (effective-property-hierarchy-value headings name
                                            global-value global-found-p)
      (when values
        (return-from effective-property
          (if plus-p
              (values (format nil "~{~A~^~%~}" values) sources)
              (values (car values) (car (last sources))))))
      (values nil nil))))

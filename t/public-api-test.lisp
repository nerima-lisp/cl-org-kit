;;;; t/public-api-test.lisp
;;;;
;;;; Pin the exported API so changes are deliberate and semver-classified.
(in-package #:cl-org-kit/test)

(defparameter +public-api+
  '(
    ;; Node types
    "NODE-TYPE" "NODE-TYPES" "NODE-TYPE-P" "NODE-TYPE-CATEGORY"
    ;; Conditions
    "ORG-KIT-ERROR" "ORG-KIT-ERROR-MESSAGE"
    "ORG-PARSE-ERROR" "ORG-PARSE-ERROR-OFFSET" "ORG-PARSE-ERROR-LINE"
    "ORG-PARSE-ERROR-COLUMN" "ORG-PARSE-ERROR-CONTEXT" "ORG-PARSE-ERROR-SOURCE-NAME"
    "ORG-QUERY-ERROR" "ORG-QUERY-ERROR-QUERY" "ORG-QUERY-ERROR-POSITION"
    "ORG-EDIT-ERROR" "ORG-EDIT-ERROR-TARGET" "ORG-EDIT-ERROR-REASON"
    "ORG-TABLE-UNSUPPORTED-SYNTAX"
    "ORG-TABLE-UNSUPPORTED-SYNTAX-FORMULA"
    "ORG-TABLE-UNSUPPORTED-SYNTAX-POSITION"
    "ORG-TABLE-UNSUPPORTED-SYNTAX-FEATURE"
    ;; T2 table alignment
    "ORG-TABLE-ERROR" "ORG-TABLE-ERROR-TARGET" "ORG-TABLE-ERROR-REASON"
    "ORG-RESOURCE-LIMIT-ERROR" "ORG-RESOURCE-LIMIT-ERROR-LIMIT-NAME"
    "ORG-RESOURCE-LIMIT-ERROR-LIMIT" "ORG-RESOURCE-LIMIT-ERROR-ACTUAL"
    "ORG-CALLBACK-ERROR" "ORG-CALLBACK-ERROR-KIND" "ORG-CALLBACK-ERROR-NAME"
    "ORG-CALLBACK-ERROR-REASON" "ORG-ELISP-EVALUATOR-ERROR" "ORG-CALLBACK-TIMEOUT-ERROR"
    "ORG-WRITE-PLAN-ERROR" "ORG-WRITE-PLAN-ERROR-PATH"
    "ORG-WRITE-PLAN-ERROR-REASON" "ORG-WRITE-PLAN-ERROR-CAUSE"
    "ORG-ID-ERROR" "ORG-ID-ERROR-REASON"
    "ORG-LINK-RESOLUTION-ERROR" "ORG-LINK-RESOLUTION-ERROR-REASON"
    "ORG-LINK-RESOLUTION-ERROR-CONTEXT"
    "ORG-STORE-LINK-ERROR" "ORG-STORE-LINK-ERROR-REASON"
    "ORG-STORE-LINK-ERROR-CONTEXT"
    ;; Clock aggregation
    "ORG-CLOCK-ERROR" "ORG-CLOCK-ERROR-REASON" "ORG-CLOCK-ERROR-CONTEXT"
    "CLOCK-SUMMARY" "MAKE-CLOCK-SUMMARY" "CLOCK-SUMMARY-P"
    "CLOCK-SUMMARY-SCOPED-MINUTES" "CLOCK-SUMMARY-UNSCOPED-MINUTES"
    "CLOCK-SUMMARY-TOTAL-MINUTES" "CLOCK-SUMMARY-CLIPPED-INTERVAL-COUNT"
    "SUM-CLOCKS"
    "CLOCKTABLE-ROW" "MAKE-CLOCKTABLE-ROW" "CLOCKTABLE-ROW-P"
    "CLOCKTABLE-ROW-LEVEL" "CLOCKTABLE-ROW-TITLE" "CLOCKTABLE-ROW-TAGS"
    "CLOCKTABLE-ROW-RANGE" "CLOCKTABLE-ROW-MINUTES"
    "CLOCKTABLE-RESULT" "MAKE-CLOCKTABLE-RESULT" "CLOCKTABLE-RESULT-P"
    "CLOCKTABLE-RESULT-TOTAL-MINUTES" "CLOCKTABLE-RESULT-ROWS"
    "CLOCKTABLE-RESULT-RANGE" "CLOCKTABLE-DATA"
    "CLOCK-CONSISTENCY-ISSUE" "MAKE-CLOCK-CONSISTENCY-ISSUE"
    "CLOCK-CONSISTENCY-ISSUE-P" "CLOCK-CONSISTENCY-ISSUE-REASON"
    "CLOCK-CONSISTENCY-ISSUE-LINE" "CLOCK-CONSISTENCY-ISSUE-OTHER-LINE"
    "CLOCK-CONSISTENCY-ISSUE-DURATION-MINUTES" "CHECK-CLOCK-CONSISTENCY"
    "TIMER-ITEM-VALUE" "INSERT-TIMER-ITEM"
    ;; Clock editing
    "CLOCK-IN" "CLOCK-OUT"
    "WRITE-CLOCKTABLE"
    ;; Edit results
    "EDIT-SPAN" "MAKE-EDIT-SPAN" "EDIT-SPAN-P" "EDIT-SPAN-START" "EDIT-SPAN-END"
    "EDIT-RESULT" "MAKE-EDIT-RESULT" "EDIT-RESULT-P"
    "EDIT-RESULT-UPDATED-DOCUMENT" "EDIT-RESULT-SPANS"
    ;; TODO transitions
    "ORG-WORKFLOW-OPTIONS" "MAKE-ORG-WORKFLOW-OPTIONS" "ORG-WORKFLOW-OPTIONS-P"
    "ORG-WORKFLOW-OPTIONS-LOG-DONE" "ORG-WORKFLOW-OPTIONS-LOG-REPEAT"
    "ORG-WORKFLOW-OPTIONS-REPEAT-TO-STATE"
    "ORG-WORKFLOW-OPTIONS-LOG-INTO-DRAWER"
    "ORG-WORKFLOW-OPTIONS-CLOCK-INTO-DRAWER"
    "TRANSITION-RESULT" "MAKE-TRANSITION-RESULT" "TRANSITION-RESULT-P"
    "TRANSITION-RESULT-PREVIOUS-STATE" "TRANSITION-RESULT-NEW-STATE"
    "TRANSITION-RESULT-LOG-EVENTS"
    "TRANSITION-RESULT-RESCHEDULED-TIMESTAMPS"
    "TRANSITION-RESULT-CLOSED-TIMESTAMP"
    "CYCLE-HEADING-TODO" "TRANSITION-HEADING-TODO"
    ;; Resource limits
    "LIMITS" "MAKE-LIMITS" "LIMITS-P" "LIMIT-VALUE"
    ;; Explicit callback registry
    "CALLBACK-REGISTRY" "MAKE-CALLBACK-REGISTRY" "CALLBACK-REGISTRY-P"
    "REGISTER-CALLBACK" "INVOKE-CALLBACK" "EVALUATE-ELISP"
    ;; Write plans
    "WRITE-PLAN-ENTRY" "MAKE-WRITE-PLAN-ENTRY" "WRITE-PLAN-ENTRY-P"
    "WRITE-PLAN-ENTRY-PATH" "WRITE-PLAN-ENTRY-CONTENT" "WRITE-PLAN-ENTRY-MODE"
    "WRITE-PLAN-ENTRY-CREATE-DIRECTORIES-P"
    "WRITE-PLAN" "MAKE-WRITE-PLAN" "WRITE-PLAN-P" "WRITE-PLAN-ENTRIES"
    "APPLY-WRITE-PLAN"
    ;; Documents
    "DOCUMENT" "MAKE-DOCUMENT" "DOCUMENT-P" "DOCUMENT-TEXT" "DOCUMENT-ENCODING"
    "DOCUMENT-BOM-P" "DOCUMENT-SOURCE-NAME" "DOCUMENT-LIMITS" "DOCUMENT-SETTINGS" "DOCUMENT-ROOT"
    "DOCUMENT-LINE-COUNT" "DOCUMENT-NEWLINE" "DOCUMENT-FINAL-NEWLINE-P"
    "OFFSET-LINE-COLUMN" "LINE-OFFSET"
    ;; Nodes
    "NODE" "MAKE-NODE" "NODE-P" "NODE-PARENT" "NODE-CHILDREN" "APPEND-CHILD"
    "NODE-BEGIN" "NODE-END" "NODE-CONTENTS-BEGIN" "NODE-CONTENTS-END"
    "NODE-POST-AFFILIATED" "NODE-POST-BLANK" "NODE-PROPERTY" "NODE-PROPERTIES"
    "NODE-ROOT" "NODE-DOCUMENT" "NODE-TEXT" "NODE-START-POSITION"
    "NODE-END-POSITION" "NODE-LINE-RANGE" "MAP-NODES"
    ;; Outline
    "OUTLINE" "OUTLINE-ENTRY" "MAKE-OUTLINE-ENTRY" "OUTLINE-ENTRY-P"
    "OUTLINE-ENTRY-KIND" "OUTLINE-ENTRY-NAME" "OUTLINE-ENTRY-LEVEL"
    "OUTLINE-ENTRY-LINE" "OUTLINE-ENTRY-END-LINE" "OUTLINE-ENTRY-NODE"
    "OUTLINE-RESULT" "MAKE-OUTLINE-RESULT" "OUTLINE-RESULT-P"
    "OUTLINE-RESULT-PATH" "OUTLINE-RESULT-LANG" "OUTLINE-RESULT-SYMBOLS"
    "OUTLINE-RESULT-TOTAL" "OUTLINE-RESULT-TRUNCATED"
    ;; Reading and writing
    "PARSE-ORG" "WRITE-ORG" "EMIT-ORG"
    ;; Format-preserving headline edits
    "SET-HEADING-TODO" "SET-HEADING-PRIORITY" "ADJUST-HEADING-PRIORITY"
    "REPLACE-HEADING-TITLE"
    "ADD-HEADING-TAGS" "REMOVE-HEADING-TAGS" "REPLACE-HEADING-TAGS"
    ;; Format-preserving metadata edits
    "SET-PROPERTY" "REMOVE-PROPERTY" "SET-PLANNING" "REMOVE-PLANNING"
    "SET-KEYWORD-VALUE" "REMOVE-KEYWORD"
    ;; Format-preserving element edits
    "REPLACE-BLOCK-BODY" "SET-CHECKBOX-STATE"
    ;; Format-preserving plain list edits
    "INSERT-LIST-ITEM" "DELETE-LIST-ITEM" "MOVE-LIST-ITEM"
    "INDENT-LIST-ITEM" "UNINDENT-LIST-ITEM" "SET-LIST-BULLET"
    "RENUMBER-ORDERED-LIST"
    ;; Format-preserving footnote edits
    "INSERT-FOOTNOTE" "RENUMBER-FOOTNOTES" "NORMALIZE-FOOTNOTES"
    ;; Format-preserving drawer edits
    "SET-DRAWER-BODY" "REMOVE-DRAWER"
    ;; T2 table alignment
    "TABLE-ALIGN" "TABLE-INSERT-ROW" "TABLE-DELETE-ROW" "TABLE-MOVE-ROW"
    "TABLE-INSERT-COLUMN" "TABLE-DELETE-COLUMN" "TABLE-MOVE-COLUMN"
    ;; T5b stable table sort
    "TABLE-SORT-ROWS"
    ;; Statistics cookies
    "STATISTICS-COOKIE-UPDATE" "MAKE-STATISTICS-COOKIE-UPDATE"
    "STATISTICS-COOKIE-UPDATE-P" "STATISTICS-COOKIE-UPDATE-NODE"
    "STATISTICS-COOKIE-UPDATE-OLD-VALUE" "STATISTICS-COOKIE-UPDATE-NEW-VALUE"
    "UPDATE-STATISTICS-COOKIES"
    "TABLE-RECALCULATE"
    ;; Format-preserving subtree edits
    "INSERT-SUBTREE" "DELETE-SUBTREE" "REPLACE-SUBTREE"
    "PROMOTE-SUBTREE" "DEMOTE-SUBTREE"
    ;; Unit E subtree relocation and sort
    "RELOCATION-RESULT" "MAKE-RELOCATION-RESULT" "RELOCATION-RESULT-P"
    "RELOCATION-RESULT-SOURCE-EDIT" "RELOCATION-RESULT-TARGET-EDIT"
    "RELOCATION-RESULT-MODE" "RELOCATION-RESULT-SOURCE-PATH"
    "RELOCATION-RESULT-TARGET-PATH"
    "MOVE-SUBTREE" "REFILE-SUBTREE" "SET-HEADING-ARCHIVE-TAG"
    "ARCHIVE-SUBTREE-TO" "SORT-SUBTREE-CHILDREN"
    ;; Queries
    "QUERY-CONTEXT" "MAKE-QUERY-CONTEXT" "QUERY-CONTEXT-P"
    "QUERY-CONTEXT-TAG-GROUPS" "QUERY-CONTEXT-PROPERTY-INHERITANCE-P"
    "QUERY-CONTEXT-TAG-INHERITANCE-P" "QUERY-CONTEXT-FILE" "QUERY-CONTEXT-CATEGORY"
    "QUERY-CONTEXT-BLOCKED-PROVIDER" "QUERY-CONTEXT-CLOCK-SUM-PROVIDER"
    "QUERY-CONTEXT-CLOCK-SUM-TODAY-PROVIDER" "QUERY-CONTEXT-CASE-FOLD-P"
    "QUERY-LIST" "QUERY-PATH" "QUERY-MATCH-AT" "QUERY-MATCH-NOW"
    "NODE-HEADING-PATH"
    ;; Timestamp values
    "TIMESTAMP-START" "TIMESTAMP-END" "TIMESTAMP-REPEATER" "TIMESTAMP-WARNING-DELAY"
    ;; Unit V timestamp facade and time context
    "ORG-TIME-ERROR" "ORG-TIME-ERROR-REASON" "ORG-TIME-ERROR-CONTEXT"
    "ORG-TIME-VALUE" "MAKE-ORG-TIME-VALUE" "ORG-TIME-VALUE-P"
    "ORG-TIME-VALUE-KIND" "ORG-TIME-VALUE-ISO8601" "ORG-TIME-VALUE-YEAR"
    "ORG-TIME-VALUE-MONTH" "ORG-TIME-VALUE-DAY" "ORG-TIME-VALUE-HOUR"
    "ORG-TIME-VALUE-MINUTE" "ORG-TIME-VALUE-SECOND" "ORG-TIME-VALUE-OFFSET-MINUTES"
    "ORG-TIME-ZONE" "MAKE-FIXED-TIME-ZONE" "RESOLVE-TIME-ZONE" "ORG-TIME-ZONE-P"
    "ORG-TIME-ZONE-NAME" "ORG-TIME-ZONE-OFFSET-MINUTES"
    "ORG-TIME-CONTEXT" "MAKE-TIME-CONTEXT" "ORG-TIME-CONTEXT-P"
    "ORG-TIME-CONTEXT-REFERENCE-TIME" "ORG-TIME-CONTEXT-ZONE"
    "ORG-TIME-CONTEXT-DST-POLICY" "ORG-TIME-CONTEXT-REFERENCE-INSTANT"
    "ORG-TIME-RANGE" "MAKE-TIME-RANGE" "ORG-TIME-RANGE-P"
    "ORG-TIME-RANGE-START" "ORG-TIME-RANGE-END"
    "ORG-TIME-RANGE-INCLUDE-START-P" "ORG-TIME-RANGE-INCLUDE-END-P"
    "ORG-TIMESTAMP-VIEW" "MAKE-ORG-TIMESTAMP-VIEW" "ORG-TIMESTAMP-VIEW-P"
    "ORG-TIMESTAMP-VIEW-KIND" "ORG-TIMESTAMP-VIEW-START" "ORG-TIMESTAMP-VIEW-END"
    "ORG-TIMESTAMP-VIEW-START-INCLUSIVE-P" "ORG-TIMESTAMP-VIEW-END-INCLUSIVE-P"
    "ORG-TIMESTAMP-VIEW-REPEATER-AMOUNT" "ORG-TIMESTAMP-VIEW-REPEATER-UNIT"
    "ORG-TIMESTAMP-VIEW-REPEATER-MODE" "ORG-TIMESTAMP-VIEW-WARNING-AMOUNT"
    "ORG-TIMESTAMP-VIEW-WARNING-UNIT" "ORG-TIMESTAMP-VIEW-WARNING-MODE"
    "TIMESTAMP-VIEW"
    "ORG-TIME-INTERVAL-KIND" "ORG-TIME-INTERVAL-AMOUNT" "ORG-TIME-INTERVAL-UNIT"
    ;; F agenda
    "ORG-AGENDA-ERROR" "ORG-AGENDA-ERROR-REASON" "ORG-AGENDA-ERROR-CONTEXT"
    "AGENDA-ENTRY" "MAKE-AGENDA-ENTRY" "AGENDA-ENTRY-P"
    "AGENDA-ENTRY-DATE" "AGENDA-ENTRY-KIND" "AGENDA-ENTRY-HEADLINE"
    "AGENDA-ENTRY-HEADING-PATH" "AGENDA-ENTRY-TIMESTAMP"
    "AGENDA-ENTRY-WARNING-STATE" "AGENDA-ENTRY-SCHEDULED-DELAY-STATE"
    "AGENDA-ENTRY-TODO-STATE" "AGENDA-ENTRY-EFFECTIVE-TAGS"
    "AGENDA-ENTRY-HABIT-P" "AGENDA-ENTRY-HABIT-CONSISTENCY"
    "AGENDA-ENTRY-SOURCE-NODE"
    "AGENDA-RESULT" "MAKE-AGENDA-RESULT" "AGENDA-RESULT-P"
    "AGENDA-RESULT-ENTRIES" "AGENDA-RESULT-START-DATE"
    "AGENDA-RESULT-END-DATE" "AGENDA-RESULT-TIME-CONTEXT"
    "AGENDA-MATCH-ENTRY" "MAKE-AGENDA-MATCH-ENTRY" "AGENDA-MATCH-ENTRY-P"
    "AGENDA-MATCH-ENTRY-DOCUMENT" "AGENDA-MATCH-ENTRY-NODE"
    "AGENDA-MATCH-ENTRY-HEADING-PATH" "AGENDA-MATCH-ENTRY-LINE"
    "AGENDA-MATCH-ENTRY-END-LINE"
    "AGENDA" "AGENDA-TODO-LIST" "AGENDA-MATCH"
    ;; Effective Org semantics
    "EFFECTIVE-HEADING-TAGS" "EFFECTIVE-PROPERTY" "STARTUP-OPTIONS"
    ;; Org IDs
    "ORG-ID-INDEX" "MAKE-ORG-ID-INDEX" "ORG-ID-INDEX-P"
    "ORG-ID-INDEX-CANDIDATES" "GENERATE-ORG-ID" "SET-HEADING-ID"
    ;; Link resolution and storage
    "LINK-RESOLUTION" "MAKE-LINK-RESOLUTION" "LINK-RESOLUTION-P"
    "LINK-RESOLUTION-STATUS" "LINK-RESOLUTION-TARGET"
    "LINK-RESOLUTION-CANDIDATES" "RESOLVE-LINK" "STORE-LINK"
    ;; Unit E1 export context and conditions
    "EXPORT-CONTEXT" "MAKE-EXPORT-CONTEXT" "EXPORT-CONTEXT-P"
    "EXPORT-CONTEXT-TITLE" "EXPORT-CONTEXT-AUTHOR" "EXPORT-CONTEXT-DATE"
    "EXPORT-CONTEXT-LANGUAGE" "EXPORT-CONTEXT-TIME-CONTEXT"
    "ORG-EXPORT-ERROR" "ORG-EXPORT-ERROR-BACKEND" "ORG-EXPORT-ERROR-REASON"
    "ORG-EXPORT-ERROR-SOURCE"
    "ORG-EXPORT-UNSUPPORTED-SYNTAX"
    "ORG-EXPORT-UNSUPPORTED-SYNTAX-BACKEND"
    "ORG-EXPORT-UNSUPPORTED-SYNTAX-SOURCE"
    "ORG-EXPORT-UNSUPPORTED-SYNTAX-POSITION"
    "ORG-EXPORT-UNSUPPORTED-SYNTAX-FEATURE"
    ;; Unit E2 HTML export
    "EXPORT-ORG")
  "Every symbol the ORG-KIT package exports. The node contract is documented in
docs/src/reference/node-model.md.")

(defun external-symbol-names ()
  (let ((names '()))
    (do-external-symbols (symbol (find-package "ORG-KIT"))
      (push (symbol-name symbol) names))
    names))

(defparameter +generated-structure-function-owners+
  '((callback-registry-p . callback-registry)
    (write-plan-entry-p . write-plan-entry)
    (write-plan-p . write-plan)
    (make-org-workflow-options . org-workflow-options)
    (org-id-index-p . org-id-index)
    (make-link-resolution . link-resolution)
    (link-resolution-p . link-resolution)
    (link-resolution-status . link-resolution)
    (link-resolution-target . link-resolution)
    (link-resolution-candidates . link-resolution)
    (org-workflow-options-p . org-workflow-options)
    (org-workflow-options-log-done . org-workflow-options)
    (org-workflow-options-log-repeat . org-workflow-options)
    (org-workflow-options-repeat-to-state . org-workflow-options)
    (org-workflow-options-log-into-drawer . org-workflow-options)
    (org-workflow-options-clock-into-drawer . org-workflow-options)
    (make-transition-result . transition-result)
    (transition-result-p . transition-result)
    (transition-result-previous-state . transition-result)
    (transition-result-new-state . transition-result)
    (transition-result-log-events . transition-result)
    (transition-result-rescheduled-timestamps . transition-result)
    (transition-result-closed-timestamp . transition-result)
    (org-time-value-p . org-time-value)
    (org-time-zone-p . org-time-zone)
    (org-time-zone-name . org-time-zone)
    (org-time-zone-offset-minutes . org-time-zone)
    (org-time-context-p . org-time-context)
    (org-time-context-reference-time . org-time-context)
    (org-time-context-zone . org-time-context)
    (org-time-context-dst-policy . org-time-context)
    (org-time-context-reference-instant . org-time-context)
    (org-time-range-p . org-time-range)
    (org-time-range-start . org-time-range)
    (org-time-range-end . org-time-range)
    (org-time-range-include-start-p . org-time-range)
    (org-time-range-include-end-p . org-time-range)
    (org-timestamp-view-p . org-timestamp-view)
    (org-timestamp-view-kind . org-timestamp-view)
    (org-timestamp-view-start . org-timestamp-view)
    (org-timestamp-view-end . org-timestamp-view)
    (org-timestamp-view-start-inclusive-p . org-timestamp-view)
    (org-timestamp-view-end-inclusive-p . org-timestamp-view)
    (org-timestamp-view-repeater-amount . org-timestamp-view)
    (org-timestamp-view-repeater-unit . org-timestamp-view)
    (org-timestamp-view-repeater-mode . org-timestamp-view)
    (org-timestamp-view-warning-amount . org-timestamp-view)
    (org-timestamp-view-warning-unit . org-timestamp-view)
    (org-timestamp-view-warning-mode . org-timestamp-view)))

(defun public-function-documentation (symbol)
  (or (documentation symbol 'function)
      (let ((owner (cdr (assoc symbol +generated-structure-function-owners+))))
        (and owner (documentation owner 'type)))))

(defun documented-p (symbol)
  "True when SYMBOL or its owning structure documents every role it fills."
  (let ((roles '()))
    (when (fboundp symbol)
      (push (public-function-documentation symbol) roles))
    (when (boundp symbol)
      (push (documentation symbol 'variable) roles))
    (when (find-class symbol nil)
      (push (documentation symbol 'type) roles))
    (and roles (every #'identity roles))))

(describe "public API surface"
  (it "exports exactly the pinned symbols"
    (let ((actual (sort (external-symbol-names) #'string<))
          (expected (sort (copy-list +public-api+) #'string<)))
      (with-soft-assertions
        (expect (set-difference actual expected :test #'string=) :to-equal '())
        (expect (set-difference expected actual :test #'string=) :to-equal '()))))

  (it "documents every exported symbol"
    (with-soft-assertions
      (dolist (name (sort (external-symbol-names) #'string<))
        (expect (list name (and (documented-p (find-symbol name "ORG-KIT")) t))
                :to-equal (list name t)))))

  (it "pins the T2, T3, and table formula API documentation"
    (with-soft-assertions
      (expect (documentation 'table-align 'function)
              :to-equal "Align a pipe table and return its validated edit result.")
      (expect (documentation 'table-insert-row 'function)
              :to-equal "Insert a row at a one-based table position and preserve unaffected source text.")
      (expect (documentation 'table-delete-row 'function)
              :to-equal "Delete a row by its one-based table position and preserve unaffected source text.")
      (expect (documentation 'table-move-row 'function)
              :to-equal "Move a row between one-based positions within its hline-delimited section.")
      (expect (documentation 'table-insert-column 'function)
              :to-equal "Insert a column at a one-based position and optionally provide values per row.")
      (expect (documentation 'table-delete-column 'function)
              :to-equal "Delete a column by its one-based table position and remap supported references.")
      (expect (documentation 'table-move-column 'function)
              :to-equal "Move a column between one-based positions and remap supported references.")
      (expect (documentation 'table-recalculate 'function)
              :to-equal "Recalculate a pipe table's TBLFM formulas and return its validated edit result.")
      (expect (documentation 'org-table-error 'type)
              :to-equal
              "Signalled when a table edit fails or a table formula cannot be evaluated.
TARGET describes the addressed table and REASON classifies the table-specific failure.")))

  (it "pins the T5b public API documentation"
    (with-soft-assertions
      (expect (documentation 'table-sort-rows 'function)
              :to-equal
              "Stable-sort rows in an Org pipe table and return its validated edit result.")))

  (it "pins priority adjustment documentation"
    (expect (documentation 'adjust-heading-priority 'function)
            :to-equal
            "Move a headline priority one step up or down within its PRIORITIES range.
An unset priority starts at the default, or wraps when PREVIOUS-DIRECTION equals DIRECTION."))

  (it "pins habit consistency documentation"
    (expect (documentation 'agenda-entry-habit-consistency 'function)
            :to-equal
            "Return habit consistency rows for ENTRY, or NIL for other agenda rows.

Each row is a plist with :DATE as a date-only ORG-TIME-VALUE and :STATUS as
:COMPLETED, :WITHIN-SCHEDULE, or :LATE. A DONE state log timestamp, or CLOSED
on a currently done heading, marks its date completed. The current SCHEDULED
timestamp anchors the active occurrence and sets its due date; this is the due
date after the latest recorded completion. Earlier due dates are reconstructed
from completion dates according to repeater mode. Dates after their due date are :LATE."))

  (it "pins the H2 clocktable writer documentation"
    (expect (documentation 'write-clocktable 'function)
            :to-equal
            "Replace a clocktable dynamic block body with RESULT's fixed table data.")
    (expect (fboundp 'clocktable-result-rows) :to-be-truthy)
    (expect (fboundp 'clocktable-result-total-minutes) :to-be-truthy)
    (expect (fboundp 'clocktable-result-range) :to-be-truthy))

  (it "pins the P8b callback and write-plan API documentation"
    (with-soft-assertions
      (expect (documentation 'make-callback-registry 'function)
              :to-equal "Return an empty callback registry with no callbacks pre-registered.")
      (expect (documentation 'register-callback 'function)
              :to-equal
              "Register FUNCTION under KIND and NAME in the caller-owned REGISTRY.
KIND is case-folded to uppercase and NAME to lowercase; a later registration
for the same key replaces the earlier callback.")
      (expect (documentation 'invoke-callback 'function)
              :to-equal
              "Invoke the callback registered under KIND and NAME with ARGUMENTS.
Missing callbacks signal ORG-CALLBACK-ERROR. Callback conditions propagate
unchanged.")
      (expect (documentation 'evaluate-elisp 'function)
              :to-equal
              "Pass EXPRESSION unchanged to the registered Emacs Lisp evaluator.
The callback is registered as KIND :EVALUATOR and NAME :EMACS-LISP. This
library does not read or evaluate EXPRESSION.")
      (expect (documentation 'make-write-plan-entry 'function)
              :to-equal
              "Return a write-plan entry without touching the filesystem.
PATH is relative. String CONTENT is encoded as UTF-8; an octet vector is copied
and written unchanged. If MODE is omitted, an existing regular file's mode is
preserved and a new file uses 0644. An explicit MODE is applied exactly,
regardless of the process umask. CREATE-DIRECTORIES-P controls creation of
missing parent directories.")
      (expect (documentation 'write-plan-entry-content 'function)
              :to-equal "Return a copy of ENTRY's text string or octet-vector content.")
      (expect (documentation 'apply-write-plan 'function)
              :to-equal
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
check.")))

  (it "pins the E public API documentation"
    (with-soft-assertions
      (expect (documentation 'relocation-result 'type)
              :to-equal
              "The validated edit results and heading paths for a relocation.")
      (expect (documentation 'make-relocation-result 'function)
              :to-equal
              "Return a relocation result containing the validated edits and paths.")
      (expect (documentation 'relocation-result-p 'function)
              :to-equal "Return true when OBJECT is a relocation result.")
      (expect (documentation 'move-subtree 'function)
              :to-equal
              "Move HEADING one sibling position UP or DOWN and return its edit result.")
      (expect (documentation 'refile-subtree 'function)
              :to-equal
              "Move or copy HEADING to the last-child position in TARGET-DOCUMENT.")
      (expect (documentation 'set-heading-archive-tag 'function)
              :to-equal "Add or remove the local ARCHIVE tag on HEADING.")
      (expect (documentation 'archive-subtree-to 'function)
              :to-equal
              "Move HEADING to the archive root or TARGET-HEADING's last-child position.")
      (expect (documentation 'sort-subtree-children 'function)
              :to-equal
              "Sort HEADING's direct headline children by a built-in Org key.
Missing values stay last in both directions, and ties retain source order.")))

  (it "keeps the package name stable and nickname-free"
    (expect (package-name (find-package "ORG-KIT")) :to-equal "ORG-KIT")
    (expect (package-nicknames (find-package "ORG-KIT")) :to-equal '())))

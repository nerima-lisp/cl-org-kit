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
    "ORG-BIBTEX-ERROR" "ORG-BIBTEX-ERROR-REASON"
    "ORG-BIBTEX-ERROR-POSITION" "ORG-BIBTEX-ERROR-CONTEXT"
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
    ;; Dynamic block updates
    "ORG-DYNAMIC-BLOCK-ERROR" "ORG-DYNAMIC-BLOCK-ERROR-REASON"
    "ORG-DYNAMIC-BLOCK-ERROR-NAME" "ORG-DYNAMIC-BLOCK-ERROR-VALUE"
    "ORG-CLOCKTABLE-PARAMETER-ERROR" "ORG-CLOCKTABLE-PARAMETER-ERROR-REASON"
    "ORG-CLOCKTABLE-PARAMETER-ERROR-PARAMETER"
    "ORG-CLOCKTABLE-PARAMETER-ERROR-VALUE"
    "REGISTER-DYNAMIC-BLOCK-HANDLER" "UPDATE-DYNAMIC-BLOCK"
    ;; Column view
    "ORG-COLUMN-VIEW-ERROR" "ORG-COLUMN-VIEW-ERROR-REASON"
    "ORG-COLUMN-VIEW-ERROR-CONTEXT" "COLUMN-VIEW-DATA"
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
    ;; Babel header arguments
    "RESOLVE-BABEL-PARAMETERS" "RESOLVE-BABEL-VARIABLES" "EXPAND-BABEL-NOWEB"
    "BIBTEX-IMPORT" "BIBTEX-EXPORT"
    ;; Babel execution
    "BABEL-TANGLE" "BABEL-DETANGLE" "UPDATE-BABEL-RESULTS"
    "REGISTER-BABEL-EXECUTOR"
    "MAKE-BABEL-SHELL-REFERENCE-EXECUTOR"
    "MAKE-BABEL-COMMON-LISP-REFERENCE-EXECUTOR"
    "EXECUTE-BABEL-SOURCE-BLOCK" "EXECUTE-BABEL-CALL"
    "BABEL-EXECUTION-RESULT" "BABEL-EXECUTION-RESULT-P"
    "BABEL-EXECUTION-RESULT-STATUS" "BABEL-EXECUTION-RESULT-VALUE"
    "BABEL-EXECUTION-RESULT-HASH" "BABEL-EXECUTION-RESULT-EDIT-RESULT"
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
    "TABLE-PLOT-WRITE-PLAN"
    "TABLE-IMPORT"
    "TABLE-EXPORT"
    "TABLE-SEND-RADIO"
    "TABLE-TRANSPOSE"
    ;; Format-preserving subtree edits
    "INSERT-SUBTREE" "DELETE-SUBTREE" "REPLACE-SUBTREE"
    "CLONE-SUBTREE-WITH-TIME-SHIFT"
    "TOGGLE-FIXED-WIDTH"
    "TOGGLE-ITEM"
    "TOGGLE-HEADING"
    "TOGGLE-HEADING-COMMENT"
    "INSERT-STRUCTURE-TEMPLATE"
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
    ;; Effort estimates
    "ORG-EFFORT-ERROR" "ORG-EFFORT-ERROR-REASON"
    "EFFORT-DURATION-MINUTES" "SUBTREE-EFFORT-MINUTES"
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
    "ADJUST-TIMESTAMP"
    "TOGGLE-TIMESTAMP-ACTIVE"
    "ORG-TIME-INTERVAL-KIND" "ORG-TIME-INTERVAL-AMOUNT" "ORG-TIME-INTERVAL-UNIT"
    ;; F agenda
    "ORG-AGENDA-ERROR" "ORG-AGENDA-ERROR-REASON" "ORG-AGENDA-ERROR-CONTEXT"
    "ORG-BABEL-ERROR" "ORG-BABEL-ERROR-REASON" "ORG-BABEL-ERROR-CONTEXT"
    "AGENDA-ENTRY" "MAKE-AGENDA-ENTRY" "AGENDA-ENTRY-P"
    "AGENDA-ENTRY-DATE" "AGENDA-ENTRY-KIND" "AGENDA-ENTRY-HEADLINE"
    "AGENDA-ENTRY-HEADING-PATH" "AGENDA-ENTRY-TIMESTAMP"
    "AGENDA-ENTRY-WARNING-STATE" "AGENDA-ENTRY-SCHEDULED-DELAY-STATE"
    "AGENDA-ENTRY-TODO-STATE" "AGENDA-ENTRY-EFFECTIVE-TAGS" "AGENDA-ENTRY-CATEGORY"
    "AGENDA-ENTRY-HABIT-P" "AGENDA-ENTRY-HABIT-CONSISTENCY"
    "AGENDA-ENTRY-SOURCE-NODE"
    "AGENDA-RESULT" "MAKE-AGENDA-RESULT" "AGENDA-RESULT-P"
    "AGENDA-RESULT-ENTRIES" "AGENDA-RESULT-START-DATE"
    "AGENDA-RESULT-END-DATE" "AGENDA-RESULT-TIME-CONTEXT"
    "AGENDA-MATCH-ENTRY" "MAKE-AGENDA-MATCH-ENTRY" "AGENDA-MATCH-ENTRY-P"
    "AGENDA-MATCH-ENTRY-DOCUMENT" "AGENDA-MATCH-ENTRY-NODE"
    "AGENDA-MATCH-ENTRY-HEADING-PATH" "AGENDA-MATCH-ENTRY-LINE"
    "AGENDA-MATCH-ENTRY-END-LINE"
    "AGENDA-BULK-ACTION"
    "AGENDA" "AGENDA-TODO-LIST" "AGENDA-MATCH" "AGENDA-SEARCH-VIEW" "AGENDA-STUCK-PROJECTS" "AGENDA-TIME-GRID" "AGENDA-TIME-GRID-ROW" "AGENDA-TIME-GRID-ROW-P" "AGENDA-TIME-GRID-ROW-DATE" "AGENDA-TIME-GRID-ROW-TIME" "AGENDA-TIME-GRID-ROW-CURRENT-P" "AGENDA-CUSTOM" "AGENDA-CUSTOM-VIEW" "AGENDA-CUSTOM-VIEW-P" "AGENDA-CUSTOM-VIEW-KEY" "AGENDA-CUSTOM-VIEW-DESCRIPTION" "AGENDA-CUSTOM-VIEW-TYPE" "AGENDA-CUSTOM-VIEW-RESULT" "AGENDA-FORMAT-TEXT" "AGENDA-TO-ORG" "AGENDA-WRITE-INTO"
    ;; Effective Org semantics
    "EFFECTIVE-HEADING-TAGS" "EFFECTIVE-PROPERTY" "STARTUP-OPTIONS"
    ;; Org IDs
    "ORG-ID-INDEX" "MAKE-ORG-ID-INDEX" "ORG-ID-INDEX-P"
    "ORG-ID-INDEX-CANDIDATES" "GENERATE-ORG-ID" "SET-HEADING-ID"
    ;; Link resolution and storage
    "LINK-RESOLUTION" "MAKE-LINK-RESOLUTION" "LINK-RESOLUTION-P"
    "LINK-RESOLUTION-STATUS" "LINK-RESOLUTION-TARGET"
    "LINK-RESOLUTION-CANDIDATES" "RESOLVE-LINK" "STORE-LINK"
    ;; FR-120 Org lint
    "ORG-LINT" "ORG-LINT-DIAGNOSTIC" "ORG-LINT-DIAGNOSTIC-P"
    "ORG-LINT-DIAGNOSTIC-CHECKER" "ORG-LINT-DIAGNOSTIC-POSITION"
    "ORG-LINT-DIAGNOSTIC-MESSAGE"
    ;; Unit E1 export context and conditions
    "EXPORT-CONTEXT" "MAKE-EXPORT-CONTEXT" "EXPORT-CONTEXT-P"
    "EXPORT-CONTEXT-TITLE" "EXPORT-CONTEXT-AUTHOR" "EXPORT-CONTEXT-DATE"
    "EXPORT-CONTEXT-LANGUAGE" "EXPORT-CONTEXT-TIME-CONTEXT"
    "EXPORT-CONTEXT-EXTENSIONS"
    "ORG-EXPORT-ERROR" "ORG-EXPORT-ERROR-BACKEND" "ORG-EXPORT-ERROR-REASON"
    "ORG-EXPORT-ERROR-SOURCE"
    "ORG-EXPORT-REGISTRATION-ERROR"
    "ORG-EXPORT-UNSUPPORTED-SYNTAX"
    "ORG-EXPORT-UNSUPPORTED-SYNTAX-BACKEND"
    "ORG-EXPORT-UNSUPPORTED-SYNTAX-SOURCE"
    "ORG-EXPORT-UNSUPPORTED-SYNTAX-POSITION"
    "ORG-EXPORT-UNSUPPORTED-SYNTAX-FEATURE"
    ;; FR-106 export backend registry
    "REGISTER-EXPORT-BACKEND" "REGISTER-DERIVED-EXPORT-BACKEND"
    "UNREGISTER-EXPORT-BACKEND"
    "REGISTER-EXPORT-LINK-TYPE" "UNREGISTER-EXPORT-LINK-TYPE"
    "EXPORT-RENDER-VIEW" "EXPORT-RENDER-NODE"
    "EXPORT-RENDER-PARENT-VIEW" "EXPORT-RENDER-PARENT-NODE"
    "EXPORT-RENDER-PARENT-RENDERER" "EXPORT-RENDER-PARENT-PREAMBLE"
    "EXPORT-RENDER-PARENT-TRAILER" "EXPORT-RENDER-PARENT-TEMPLATE"
    "EXPORT-RENDER-CONTEXT-BACKEND" "EXPORT-RENDER-CONTEXT-DOCUMENT"
    "EXPORT-RENDER-CONTEXT-METADATA" "EXPORT-RENDER-CONTEXT-OPTIONS"
    "EXPORT-RENDER-CONTEXT-LIMITS" "EXPORT-RENDER-CONTEXT-ROOT-VIEWS"
    "EXPORT-RENDER-CONTEXT-TOC" "EXPORT-RENDER-CONTEXT-FOOTNOTES"
    "EXPORT-RENDER-CONTEXT-RAW-OUTPUT-P" "EXPORT-RENDER-CONTEXT-EXTENSION"
    "EXPORT-RENDER-CONTEXT-OWNER-BACKEND" "EXPORT-RENDER-CONTEXT-STATE"
    "EXPORT-RENDER-FRAME-MODE" "EXPORT-RENDER-FRAME-TABLE-CELL-P"
    "EXPORT-RENDER-FRAME-PRESERVE-SOURCE-BREAKS-P"
    "EXPORT-RENDER-FRAME-OUTPUT-OFFSET"
    "EXPORT-OPTION-EXPLICIT-P" "EXPORT-OPTION-VALUE"
    "EXPORT-BACKEND-DERIVED-P"
    "EXPORT-VIEW-NODE" "EXPORT-VIEW-CHILDREN"
    "EXPORT-VIEW-P"
    "EXPORT-VIEW-SOURCE-DOCUMENT" "MAKE-EXPORT-VIEW"
    "MAKE-EXPORT-LINK-RESULT" "EXPORT-LINK-RESULT-KIND"
    "EXPORT-LINK-RESULT-DESTINATION" "EXPORT-LINK-RESULT-DESCRIPTION"
    ;; Export API
    "EXPORT-ORG" "EXPORT-ORG-MAN" "EXPORT-ORG-TEXINFO"
    "EXPORT-ICALENDAR-DOCUMENTS"
    ;; Org crypt
    "ORG-CRYPT-ERROR" "ORG-CRYPT-ERROR-REASON" "ORG-CRYPT-ERROR-CAUSE"
    "CRYPT-ENCRYPT-ENTRY" "CRYPT-DECRYPT-ENTRY"
    "CRYPT-ENCRYPT-ENTRIES" "CRYPT-DECRYPT-ENTRIES"
    "ORG-CAPTURE-ERROR" "ORG-CAPTURE-ERROR-REASON" "ORG-CAPTURE-ERROR-CONTEXT"
    "CAPTURE-TEMPLATE-RESULT" "MAKE-CAPTURE-TEMPLATE-RESULT"
    "CAPTURE-TEMPLATE-RESULT-P" "CAPTURE-TEMPLATE-RESULT-EXPANDED-TEXT"
    "CAPTURE-TEMPLATE-RESULT-CURSOR-OFFSET" "EXPAND-CAPTURE-TEMPLATE"
    "CAPTURE-TARGET-RESULT" "MAKE-CAPTURE-TARGET-RESULT"
    "CAPTURE-TARGET-RESULT-P" "CAPTURE-TARGET-RESULT-EDIT-RESULT"
    "CAPTURE-TARGET-RESULT-TARGET-DOCUMENT"
    "CAPTURE-TARGET-RESULT-CURSOR-OFFSET" "CAPTURE-INTO-TARGET" "MAKE-WRITE-PLAN-DELETE-ENTRY" "WRITE-PLAN-ENTRY-OPERATION" "REFILE-TARGET-CANDIDATES" "ORG-ARCHIVE-ERROR" "ORG-ARCHIVE-ERROR-REASON" "ORG-ARCHIVE-ERROR-CONTEXT" "RESOLVE-ARCHIVE-LOCATION" "ARCHIVE-TO-SIBLING" "ORG-ATTACHMENT-ERROR" "ORG-ATTACHMENT-ERROR-REASON" "ORG-ATTACHMENT-ERROR-CONTEXT" "RESOLVE-ATTACHMENT-DIRECTORY" "ADD-ATTACHMENT" "DELETE-ATTACHMENT"
)
  "Every symbol the ORG-KIT package exports. The node contract is documented in
docs/src/reference/node-model.md.")

(defun external-symbol-names ()
  (let ((names '()))
    (do-external-symbols (symbol (find-package "ORG-KIT"))
      (push (symbol-name symbol) names))
    names))

(defparameter +generated-structure-function-owners+
  '((babel-execution-result-p . babel-execution-result)
    (babel-execution-result-status . babel-execution-result)
    (babel-execution-result-value . babel-execution-result)
    (babel-execution-result-hash . babel-execution-result)
    (babel-execution-result-edit-result . babel-execution-result)
    (callback-registry-p . callback-registry)
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
    (org-timestamp-view-warning-mode . org-timestamp-view)
    (make-capture-template-result . capture-template-result)
    (capture-template-result-p . capture-template-result)
    (make-capture-target-result . capture-target-result)
    (capture-target-result-p . capture-target-result)
    (org-lint-diagnostic-p . org-lint-diagnostic)
    (org-lint-diagnostic-checker . org-lint-diagnostic)
    (org-lint-diagnostic-position . org-lint-diagnostic)
    (org-lint-diagnostic-message . org-lint-diagnostic)
    (agenda-time-grid-row-p . agenda-time-grid-row)
    (agenda-time-grid-row-date . agenda-time-grid-row)
    (agenda-time-grid-row-time . agenda-time-grid-row)
    (agenda-time-grid-row-current-p . agenda-time-grid-row)))

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

  (it "accepts the public Babel result element limit"
    (expect (limit-value (make-limits :max-babel-result-elements 7)
                         :max-babel-result-elements)
            :to-be 7))

  (it "pins the Babel detangling API documentation"
    (expect (documentation 'babel-detangle 'function)
            :to-equal
            "Return an edit result that copies linked tangling output into DOCUMENT.

TANGLED-CONTENT is scanned for Org link comments and their matching
source-name end markers. Only the matched source-block bodies are replaced;
no file is read or written and no process is started. Unmatched links are left
alone. Ambiguous source names and malformed linked sections signal
ORG-BABEL-ERROR."))

  (it "pins the Babel shell reference executor contract"
    (expect (documentation 'make-babel-shell-reference-executor 'function)
            :to-equal
            "Return an opt-in shell executor that delegates process execution to RUNNER.
SHELL is passed to RUNNER with the expanded SCRIPT, optional STDIN, and raw CMDLINE.
RUNNER must return standard output as a string. This helper starts no process.
Persistent shell sessions are unsupported; :stdin and :cmdline use one-shot scripts."))

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
              :to-equal
              "Recalculate a pipe table's TBLFM formulas and return its validated edit result.
FUNCTIONS is an alist of string names and call-scoped function designators.
CALLBACKS is a caller-owned callback registry used for quoted Lisp formulas.")
      (expect (documentation 'table-plot-write-plan 'function)
              :to-equal
              "Build a write plan for a table's plot script, data, and radar setup.
SCRIPT-PATH and DATA-PATH are required relative paths; RADAR-PATH is required
for radar plots. SCRIPT-CONTENT supplies the content named by a PLOT script
option, and BASE-DIRECTORY resolves a relative image path.
The generated script is executable gnuplot code built from document options
and any supplied SCRIPT-CONTENT. It may run commands from those sources,
including gnuplot's `system` function. Callers should pass scripts to gnuplot
only when the document and any supplied script content are trusted. This
library returns a write plan but does not create files or execute gnuplot.
The image output named by FILE may be absolute or contain `..`, so it is not
confined to the write-plan root.")
      (expect (documentation 'table-import 'function)
              :to-equal
              "Insert CSV or TSV TEXT at OFFSET as an aligned Org pipe table.
FORMAT may be :AUTO, :CSV, or :TSV. Return the validated edit result.")
      (expect (documentation 'table-export 'function)
              :to-equal
              "Export an Org pipe table as CSV or TSV text without writing a file.
FORMAT may be :AUTO, :CSV, or :TSV; :AUTO follows Org's default TSV format.")
      (expect (documentation 'table-send-radio 'function)
              :to-equal
              "Transform a radio table and replace matching RECEIVE bodies.
Return the validated edit result, preserving all source outside those bodies.")
      (expect (documentation 'table-transpose 'function)
              :to-equal
              "Transpose a rectangular Org pipe table, remove hlines, and return an edit result.
TBLFM text is preserved unchanged.")
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
from completion dates according to repeater mode. Dates after their due date
are :LATE."))

  (it "pins the H2 clocktable writer documentation"
    (expect (documentation 'write-clocktable 'function)
            :to-equal
            "Replace a clocktable block body with RESULT and optional formatting.")
    (expect (fboundp 'clocktable-result-rows) :to-be-truthy)
    (expect (fboundp 'clocktable-result-total-minutes) :to-be-truthy)
    (expect (fboundp 'clocktable-result-range) :to-be-truthy))

  (it "pins the callback and write-plan API documentation"
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
When CONTEXT is supplied, pass it as a second callback argument; calls that
omit it retain the one-argument callback contract. This function does not
parse or execute EXPRESSION.")
      (expect (documentation 'make-write-plan-entry 'function)
              :to-equal
              "Return a write-plan entry without touching the filesystem.
PATH is relative. String CONTENT is encoded as UTF-8; an octet vector is copied
and written unchanged. If MODE is omitted, an existing regular file's mode is
preserved and a new file uses 0644. An explicit MODE is applied exactly,
regardless of the process umask. CREATE-DIRECTORIES-P controls creation of
missing parent directories.")
      (expect (documentation 'write-plan-entry-content 'function)
              :to-equal "Return a copy of ENTRY's content, or NIL for a delete entry.")
      (expect (documentation 'write-plan-entry-mode 'function)
              :to-equal
              "Return the permission bits for ENTRY, or NIL for a delete entry.")
      (expect (documentation (quote make-write-plan-delete-entry) (quote function))
              :to-equal
              "Return an entry that removes an existing regular file below ROOT when applied.
Symlink components below ROOT are rejected.")
      (expect (documentation (quote write-plan-entry-operation) (quote function))
              :to-equal
              "Return ENTRY operation, either :WRITE or :DELETE.")
      (expect (documentation 'apply-write-plan 'function)
              :to-equal
              "Validate PLAN in full, then apply each write or delete entry below ROOT.
Writes create or replace targets through a same-directory temporary file and
rename; deletes unlink their targets and cannot be rolled back. ROOT must name
an existing directory; a symlink used as ROOT resolves to its target. All paths
are relative POSIX paths. Every entry and limit is checked before directory or
file creation. Returns normalized relative paths in plan order; a multi-file
plan is not transactional. The caller must prevent other actors from renaming
or replacing path components during application. The library rejects absolute
paths, `..`, NUL, and symlink components below the resolved root. After each
operation, it compares the device and inode of the opened parent with a
root-relative re-open. That check detects a missing or different parent. A
missing or different parent restored before the check is not detected. For a
delete entry, a target replaced by a symlink after preflight causes the symlink
itself to be unlinked; a target replaced by a directory signals :io-failed. The
check cannot prevent a rename during an operation or after the check.")))

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

  (it "copies export context extensions without copying their values"
    (let* ((value (list :processor-state))
           (extensions (list (cons :citation-processor value)))
           (context (make-export-context :extensions extensions))
           (readback (export-context-extensions context)))
      (setf (caar extensions) :changed)
      (expect (caar readback) :to-be :citation-processor)
      (expect (cdar readback) :to-be value)
      (setf (caar readback) :changed-again)
      (expect (caar extensions) :to-be :changed)))

  (it "copies export context metadata on input and access"
    (let* ((title (copy-seq "Stable title"))
           (context (make-export-context :title title))
           (readback (export-context-title context)))
      (setf (char title 0) #\X)
      (expect readback :to-equal "Stable title")
      (setf (char readback 0) #\Y)
      (expect (export-context-title context) :to-equal "Stable title")))

  (it "exports Texinfo and man through their public entrypoints"
    (let ((document (parse-org "Public export.")))
      (expect (export-org-texinfo document)
              :to-equal (export-org document :texinfo))
      (expect (export-org-man document :output-type :octets)
              :to-equalp (export-org document :man :output-type :octets))))
  (it "documents the Texinfo and man export entrypoints"
    (expect (documentation (quote export-org-texinfo) (quote function))
            :to-equal "Export DOCUMENT with the :TEXINFO backend.")
    (expect (documentation (quote export-org-man) (quote function))
            :to-equal "Export DOCUMENT with the man backend."))

  (it "registers derived backends and applies their filter chains"
    (let* ((document (parse-org "A custom export."))
           (base-output (export-org document :gfm))
           (seen-output nil)
           (registered-filter-p nil)
           (registered-empty-p nil)
           (later-filter-ran-p nil))
      (unwind-protect
           (progn
             (register-derived-export-backend
              :api-test-filter :gfm
              :filters
              (list
               (cons :final-output
                     (list (lambda (value context)
                             (declare (ignore context))
                             (setf seen-output value)
                             nil)
                           (lambda (value context)
                             (declare (ignore context))
                             (concatenate 'string "filtered:" value))))))
             (setf registered-filter-p t)
             (expect (export-org document :api-test-filter)
                     :to-equal (concatenate 'string "filtered:" base-output))
             (expect seen-output :to-equal base-output)
             (register-derived-export-backend
              :api-test-empty-filter :gfm
              :filters
              (list
               (cons :final-output
                     (list (lambda (value context)
                             (declare (ignore value context))
                             "")
                           (lambda (value context)
                             (declare (ignore context))
                             (setf later-filter-ran-p t)
                             value)))))
             (setf registered-empty-p t)
             (expect (export-org document :api-test-empty-filter) :to-equal "")
             (expect later-filter-ran-p :to-be nil))
        (when registered-empty-p
          (unregister-export-backend :api-test-empty-filter))
        (when registered-filter-p
          (unregister-export-backend :api-test-filter)))))

  (it "reports duplicate backend names with the registration condition"
    (let ((condition
            (handler-case
                (progn
                  (register-derived-export-backend :html :gfm)
                  nil)
              (org-export-registration-error (condition) condition))))
      (expect (typep condition 'org-export-registration-error) :to-be t)
      (when (typep condition 'org-export-registration-error)
        (expect (org-export-error-backend condition) :to-be :html)
        (expect (org-export-error-reason condition) :to-be :duplicate-backend))))

  (it "keeps the package name stable and nickname-free"
    (expect (package-name (find-package "ORG-KIT")) :to-equal "ORG-KIT")
    (expect (package-nicknames (find-package "ORG-KIT")) :to-equal '()))
  (it "rejects extension entries that are not keyword pairs"
    (let ((condition
           (handler-case
               (progn
                 (make-export-context :extensions (list :not-a-pair))
                 nil)
             (type-error (caught) caught))))
      (with-soft-assertions
        (expect (not (null condition)) :to-be t)
        (when condition
          (expect (type-error-datum condition)
                  :to-equal (list :not-a-pair))))))
  (it "validates caller context fields, views, and link results"
  (labels ((signals-type-error-p (function)
             (handler-case
                 (progn (funcall function) nil)
               (type-error () t))))
    (let* ((author (copy-seq "Caller"))
           (date (copy-seq "2030-01-02"))
           (language (copy-seq "fr"))
           (time-context
             (make-time-context :reference-time (make-org-time-value "2030-01-02")
                                :zone (make-fixed-time-zone 0)))
           (context (make-export-context :author author :date date
                                         :language language
                                         :time-context time-context))
           (source-document (parse-org "Export view source."))
           (node (make-node :section))
           (child-view (make-export-view (make-node :paragraph)))
           (children (list child-view))
           (view (make-export-view node :children children
                                   :source-document source-document))
           (destination (copy-seq "images/cover.png"))
           (description (copy-seq "Cover image"))
           (link-result (make-export-link-result :kind :image
                                                 :destination destination
                                                 :description description)))
      (setf (char author 0) #\X
            (char date 0) #\X
            (char language 0) #\X
            (char destination 0) #\X
            (char description 0) #\X)
      (with-soft-assertions
        (expect (export-context-p context) :to-be t)
        (expect (export-context-p nil) :to-be nil)
        (expect (export-context-author context) :to-equal "Caller")
        (expect (export-context-date context) :to-equal "2030-01-02")
        (expect (export-context-language context) :to-equal "fr")
        (expect (export-context-time-context context) :to-be time-context)
        (expect (signals-type-error-p
                 (lambda () (make-export-context :author 42)))
                :to-be t)
        (expect (signals-type-error-p
                 (lambda () (make-export-context :time-context :invalid)))
                :to-be t)
        (expect (signals-type-error-p
                 (lambda () (make-export-context :extensions '(("bad-key" . nil)))))
                :to-be t)
        (let ((cyclic-extensions (list (cons :processor nil))))
          (setf (cdr cyclic-extensions) cyclic-extensions)
          (expect (signals-type-error-p
                   (lambda ()
                     (make-export-context :extensions cyclic-extensions)))
                  :to-be t))
        (expect (export-view-node view) :to-be node)
        (expect (export-view-children view) :to-equal children)
        (expect (export-view-source-document view) :to-be source-document)
        (progn
          (expect (export-view-p view) :to-be t)
          (expect (export-view-p node) :to-be nil))
        (setf (car children) nil)
        (expect (first (export-view-children view)) :to-be child-view)
        (expect (signals-type-error-p
                 (lambda () (make-export-view nil)))
                :to-be t)
        (expect (signals-type-error-p
                 (lambda () (make-export-view node :children (list :not-a-view))))
                :to-be t)
        (expect (signals-type-error-p
                 (lambda () (make-export-view node :children (cons child-view :tail))))
                :to-be t)
        (expect (signals-type-error-p
                 (lambda () (make-export-view node :source-document :not-a-document)))
                :to-be t)
        (expect (signals-type-error-p (lambda () (export-view-node nil)))
                :to-be t)
        (expect (signals-type-error-p (lambda () (export-view-children nil)))
                :to-be t)
        (expect (signals-type-error-p
                 (lambda () (export-view-source-document nil)))
                :to-be t)
        (expect (export-link-result-kind link-result) :to-be :image)
        (expect (export-link-result-destination link-result)
                :to-equal "images/cover.png")
        (expect (export-link-result-description link-result)
                :to-equal "Cover image")
        (let ((returned-destination (export-link-result-destination link-result))
              (returned-description (export-link-result-description link-result)))
          (setf (char returned-destination 0) #\Y
                (char returned-description 0) #\Y)
          (expect (export-link-result-destination link-result)
                  :to-equal "images/cover.png")
          (expect (export-link-result-description link-result)
                  :to-equal "Cover image"))
        (expect (signals-type-error-p
                 (lambda () (make-export-link-result :kind :other
                                                     :destination "path"
                                                     :description "label")))
                :to-be t)
        (expect (signals-type-error-p
                 (lambda () (make-export-link-result :kind :link
                                                     :destination 42
                                                     :description "label")))
                :to-be t)
        (expect (signals-type-error-p
                 (lambda () (make-export-link-result :kind :link
                                                     :destination "path"
                                                     :description nil)))
                :to-be t)
        (expect (signals-type-error-p (lambda () (export-link-result-kind nil)))
                :to-be t)
        (expect (signals-type-error-p
                 (lambda () (export-link-result-destination nil)))
                :to-be t)
        (expect (signals-type-error-p
                 (lambda () (export-link-result-description nil)))
                :to-be t))))))

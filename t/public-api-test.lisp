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
    "ORG-RESOURCE-LIMIT-ERROR" "ORG-RESOURCE-LIMIT-ERROR-LIMIT-NAME"
    "ORG-RESOURCE-LIMIT-ERROR-LIMIT" "ORG-RESOURCE-LIMIT-ERROR-ACTUAL"
    ;; Clock aggregation
    "ORG-CLOCK-ERROR" "ORG-CLOCK-ERROR-REASON" "ORG-CLOCK-ERROR-CONTEXT"
    "CLOCK-SUMMARY" "MAKE-CLOCK-SUMMARY" "CLOCK-SUMMARY-P"
    "CLOCK-SUMMARY-SCOPED-MINUTES" "CLOCK-SUMMARY-UNSCOPED-MINUTES"
    "CLOCK-SUMMARY-TOTAL-MINUTES" "CLOCK-SUMMARY-CLIPPED-INTERVAL-COUNT"
    "SUM-CLOCKS"
    ;; Edit results
    "EDIT-SPAN" "MAKE-EDIT-SPAN" "EDIT-SPAN-P" "EDIT-SPAN-START" "EDIT-SPAN-END"
    "EDIT-RESULT" "MAKE-EDIT-RESULT" "EDIT-RESULT-P"
    "EDIT-RESULT-UPDATED-DOCUMENT" "EDIT-RESULT-SPANS"
    ;; Resource limits
    "LIMITS" "MAKE-LIMITS" "LIMITS-P" "LIMIT-VALUE"
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
    "SET-HEADING-TODO" "SET-HEADING-PRIORITY" "REPLACE-HEADING-TITLE"
    "ADD-HEADING-TAGS" "REMOVE-HEADING-TAGS" "REPLACE-HEADING-TAGS"
    ;; Format-preserving metadata edits
    "SET-PROPERTY" "REMOVE-PROPERTY" "SET-PLANNING" "REMOVE-PLANNING"
    "SET-KEYWORD-VALUE" "REMOVE-KEYWORD"
    ;; Format-preserving element edits
    "REPLACE-BLOCK-BODY" "SET-CHECKBOX-STATE"
    ;; Format-preserving subtree edits
    "INSERT-SUBTREE" "DELETE-SUBTREE" "REPLACE-SUBTREE"
    "PROMOTE-SUBTREE" "DEMOTE-SUBTREE"
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
    ;; Effective Org semantics
    "EFFECTIVE-HEADING-TAGS" "EFFECTIVE-PROPERTY")
  "Every symbol the ORG-KIT package exports. The node contract is documented in
docs/src/reference/node-model.md.")

(defun external-symbol-names ()
  (let ((names '()))
    (do-external-symbols (symbol (find-package "ORG-KIT"))
      (push (symbol-name symbol) names))
    names))

(defparameter +generated-structure-function-owners+
  '((org-time-value-p . org-time-value)
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

  (it "keeps the package name stable and nickname-free"
    (expect (package-name (find-package "ORG-KIT")) :to-equal "ORG-KIT")
    (expect (package-nicknames (find-package "ORG-KIT")) :to-equal '())))

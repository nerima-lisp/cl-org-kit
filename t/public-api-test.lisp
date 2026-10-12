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
    "TIMESTAMP-START" "TIMESTAMP-END" "TIMESTAMP-REPEATER" "TIMESTAMP-WARNING-DELAY")
  "Every symbol the ORG-KIT package exports. The node contract is documented in
docs/src/reference/node-model.md.")

(defun external-symbol-names ()
  (let ((names '()))
    (do-external-symbols (symbol (find-package "ORG-KIT"))
      (push (symbol-name symbol) names))
    names))

(defun documented-p (symbol)
  "True when SYMBOL carries a docstring in every role it actually fills."
  (let ((roles '()))
    (when (fboundp symbol)
      (push (documentation symbol 'function) roles))
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

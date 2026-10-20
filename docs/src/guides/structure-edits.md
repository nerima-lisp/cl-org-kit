# Structure Edits

Structural edits preserve source outside the changed spans. Each operation
returns an `edit-result`; use `edit-result-updated-document` for the updated
document and `edit-result-spans` for the ranges in the original source.

```lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(defun first-node-of-type (document type)
  (let ((found nil))
    (org-kit:map-nodes
     (lambda (node)
       (when (and (null found) (eq (org-kit:node-type node) type))
         (setf found node)))
     (org-kit:document-root document))
    found))
```

## Plain lists

List edits target an item or a plain-list node. `insert-list-item` accepts
source for exactly one item without a final line ending. Its item kind must
match the destination list. Relative positions for an item are `:before` and
`:after`; positions for a list are `:first` and `:last`.

```lisp
(let* ((document (org-kit:parse-org (format nil "- [ ] first~%")))
       (item (first-node-of-type document :item))
       (edit (org-kit:insert-list-item
              document item "- [X] second" :position :after)))
  (org-kit:write-org (org-kit:edit-result-updated-document edit)))
```

The list editing functions are:

- `delete-list-item` removes an item and its nested content.
- `move-list-item` moves an item before or after a sibling in the same list.
- `indent-list-item` increases indentation beneath the preceding sibling.
- `unindent-list-item` moves a nested item up one list level when its source
  boundaries are unambiguous.
- `set-list-bullet` changes the marker on each direct item in a list. It accepts
  `-`, `+`, `*`, or a numbered marker such as `1.` or `1)`.
- `renumber-ordered-list` renumbers an ordered list from `:start` (default 1)
  while retaining each marker's punctuation.

Checkboxes, description text, and marker spacing are retained by marker and
number changes. Indenting requires a preceding sibling. List moves stay within
one list. Unsafe or ambiguous edits signal `org-edit-error` with a reason such
as `:invalid-value` or `:uneditable-structure`.

## Footnotes

`insert-footnote` inserts a reference at a character offset and appends its
definition. It chooses one greater than the largest numeric label already used
by a reference or definition. It returns four values: the edit result, assigned
label, undefined reference labels, and unreferenced definition labels.
Offsets must fall in paragraph text or a headline title; block contents such as
source blocks are not valid insertion points. At a line start, it places the
reference on its own paragraph line so the following Org block syntax remains
intact. The definition body may contain Org content, but it cannot contain a
headline because that would end the definition.

```lisp
(let* ((document (org-kit:parse-org "A sentence."))
       (offset (length (org-kit:document-text document))))
  (multiple-value-bind (edit label undefined-refs unused-defs)
      (org-kit:insert-footnote document offset "A note.")
    (list label undefined-refs unused-defs
          (org-kit:write-org (org-kit:edit-result-updated-document edit)))))
```

`renumber-footnotes` renumbers numeric labels by first reference. Named labels
remain unchanged. `normalize-footnotes` moves existing definitions to the
document end in first-reference order, then keeps unreferenced definitions in
their original order. It does not renumber labels or convert inline or
anonymous footnotes. Both functions return the edit result followed by
undefined reference labels and unreferenced definition labels. Diagnostics
describe the updated document and do not prevent the edit.

## Drawers

`set-drawer-body` creates a drawer under a headline or replaces the body of its
unique matching drawer. Names are matched case-insensitively and written in
uppercase. Names use only word characters, underscores, and hyphens. The body
is raw Org content without drawer markers; it cannot contain a headline or a
closing `:END:` marker. `remove-drawer` deletes the unique matching drawer.

```lisp
(let* ((document (org-kit:parse-org (format nil "* Task~%Body.~%")))
       (headline (first-node-of-type document :headline))
       (edit (org-kit:set-drawer-body document headline "REVIEW" "Ready")))
  (org-kit:write-org (org-kit:edit-result-updated-document edit)))
```

`PROPERTIES` and `LOGBOOK` are reserved for their dedicated editing APIs.
Missing or duplicate drawer names, unclosed drawers, and invalid names or
bodies signal `org-edit-error` with a reason. Output and encoding limits apply
to all structural edits.

## Timestamps

`adjust-timestamp` changes one date, clock, repeater, or warning component by
one step. Its `delta` is `1` or `-1`; `:endpoint` selects `:start` or, for a
range, `:end`. Date changes use Org's calendar overflow behavior and update the
weekday. A clock change shifts both clock values in one endpoint's time range.
Repeater and warning components are `:repeater-amount`, `:repeater-unit`,
`:warning-amount`, and `:warning-unit`; calendar components are `:year`,
`:month`, `:day`, `:hour`, and `:minute`.

`toggle-timestamp-active` switches the delimiters around a complete timestamp
or date range. Both operations return an `edit-result` and preserve text outside
the timestamp.

```lisp
(let* ((document (org-kit:parse-org "<2024-02-28 Wed>"))
       (timestamp (first-node-of-type document :timestamp))
       (edit (org-kit:adjust-timestamp document timestamp :day 1)))
  (org-kit:write-org (org-kit:edit-result-updated-document edit)))
```

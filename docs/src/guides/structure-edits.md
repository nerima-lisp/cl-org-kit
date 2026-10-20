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
match the destination list: ordered items go in ordered lists, unordered items
go in unordered lists, and descriptive items go in descriptive lists. Relative
positions for an item are `:before` and `:after`; positions for a list are
`:first` and `:last`.

```lisp
(let* ((document (org-kit:parse-org (format nil "- [ ] first~%")))
       (item (first-node-of-type document :item))
       (edit (org-kit:insert-list-item
              document item "- [X] second" :position :after)))
  (org-kit:write-org (org-kit:edit-result-updated-document edit)))
```

The call forms and target requirements are:

- `(insert-list-item document target raw-item &key (position :after))` accepts
  an item or plain list as `target`.
- `(delete-list-item document target)` deletes an item and its nested content.
- `(move-list-item document target destination &key (position :before))` moves
  sibling items in the same list.
- `(indent-list-item document target &key (amount 2))` adds `amount` spaces
  beneath the preceding sibling; `target` must be an item.
- `(unindent-list-item document target)` moves an item out of a nested list
  when that list is the final content of its parent item. The item lands after
  the remaining nested siblings.
- `(set-list-bullet document target bullet)` changes the marker on each direct
  item; `target` can be a list or one of its items. It accepts `-`, `+`, `*`,
  or a numbered marker such as `1.` or `1)`. With a numbered marker, an
  explicit `[@N]` counter resets numbering for that item and the next one.
- `(renumber-ordered-list document target &key (start 1))` renumbers an ordered
  list from `start` while retaining each marker's punctuation. An explicit
  `[@N]` counter resets the number for that item and the following sequence.

Checkboxes, description text, and marker spacing are retained by marker and
number changes. Invalid input signals `org-edit-error` with `:invalid-value`;
indenting a first item or unindenting from a nested list that is not the final
content of its parent item signals `:uneditable-structure`.

## Footnotes

`insert-footnote` takes a zero-based character offset between source
characters, from `0` through the document length. It inserts only in paragraph
text or a headline title; block contents such as source blocks are not valid
insertion points. Offsets inside inline objects are also invalid. At a valid
line start, it puts the reference on its own line
with a leading space; at other valid offsets, it inserts the reference at that
character position. It appends the definition at the end of the document. The
leading space at line start keeps Org from parsing the reference as a
definition. The label is one greater than the largest numeric label used by
any reference or definition. The body must be nonempty valid Org source
without a trailing line ending or headline.

The function returns the edit result, assigned label, undefined reference
labels, and unreferenced definition labels.

For example, with no existing numeric labels, inserting at offset `0` in
`Paragraph text` starts the output with a separate ` [fn:1]` line before the
paragraph and appends `[fn:1] BODY` as the definition.

```lisp
(let* ((document (org-kit:parse-org "A sentence."))
       (offset (length (org-kit:document-text document))))
  (multiple-value-bind (edit label undefined-refs unused-defs)
      (org-kit:insert-footnote document offset "A note.")
    (list label undefined-refs unused-defs
          (org-kit:write-org (org-kit:edit-result-updated-document edit)))))
```

`renumber-footnotes` renumbers numeric labels in first-reference order, then
renumbers unreferenced numeric definitions in definition order. Named labels
remain unchanged. `normalize-footnotes` moves existing definitions to the
document end in first-reference order, then keeps unreferenced definitions in
their original order. It does not renumber labels or convert inline or
anonymous footnotes. Both functions return the edit result followed by
undefined reference labels and unreferenced definition labels. Diagnostics
describe the updated document and do not prevent the edit. Diagnostic values
are lists of footnote label strings.

## Drawers

`set-drawer-body` creates a drawer under a headline when no direct drawer in
that headline's section has the requested name. It replaces the body when one
matching drawer exists. Names are matched case-insensitively and written in
uppercase. Names use only word characters, underscores, and hyphens. The body
is raw Org content without drawer markers; it cannot contain a headline or a
closing `:END:` marker. `remove-drawer` deletes one matching direct drawer.

```lisp
(let* ((document (org-kit:parse-org (format nil "* Task~%Body.~%")))
       (headline (first-node-of-type document :headline))
       (edit (org-kit:set-drawer-body document headline "REVIEW" "Ready")))
  (org-kit:write-org (org-kit:edit-result-updated-document edit)))
```

`PROPERTIES` and `LOGBOOK` are reserved for their dedicated editing APIs. Use
`set-property` and `remove-property` for heading properties. Use `clock-in` and
`clock-out` to manage CLOCK entries; the drawer settings for these functions
are described in the [clock guide](clock.md#start-and-stop-a-clock).
`set-drawer-body` rejects duplicate matching drawers with `:ambiguous-target`;
`remove-drawer` also reports `:ambiguous-target` for duplicates and
`:target-not-found` when none exists. Invalid names and bodies use
`:invalid-value`; reserved names use `:unsupported-option`; an unclosed drawer
uses `:uneditable-structure`. [Resource limits](../reference/node-model.md#resource-limits)
apply to all structural edits.

## Subtree clones

`clone-subtree-with-time-shift` inserts copies of a headline subtree as
siblings. For a subtree without an active repeater, `count` is the number of
additional siblings. Set both `shift-amount` and `shift-unit` to `nil` for
unshifted copies. Otherwise, `shift-amount` must be an integer and
`shift-unit` one of `:hour`, `:day`, `:week`, `:month`, or `:year`. The first
copy shifts by that amount, the second by twice that amount, and so on. The
operation shifts timestamp tokens throughout the subtree source, including in
source, example, and verbatim text. It returns an `edit-result` and leaves the
input document unchanged.

When the subtree contains an active repeater on an active `<...>` timestamp and
`shift-amount` is an integer, the edit replaces the target with `count + 2`
instances: one unshifted instance, then `count + 1` instances shifted by
successive multiples of the amount. This also applies when the amount is zero.
Only the final instance
retains the repeater. CLOCK lines remain on the unshifted instance; when CLOCK
lines are removed from another instance, every empty drawer in that instance
is dropped. If both shift arguments are `nil`, an active repeater is left in
place and `count` unshifted copies that retain the repeater are added after the
target; CLOCK lines are removed from each copy.

Generated instances whose root has an `ID` receive new root IDs; descendant
IDs remain unchanged. Pass `:random-source` as a zero-argument function that
returns an integer from `0` through `255` per call to control those IDs.
`count` must be a non-negative integer, the target must be a headline, and the
shift arguments must be both `nil` or a valid amount and unit. Invalid
arguments signal `org-edit-error`. The document's `:max-subtree-clones` limit bounds the copies
or repeater instances produced by one edit, alongside the output and node
limits.

```lisp
(let* ((document (org-kit:parse-org (format nil "* Task~%SCHEDULED: <2026-03-02>~%")))
       (task (first-node-of-type document :headline))
       (edit (org-kit:clone-subtree-with-time-shift
              document task 2 1 :week)))
  (org-kit:write-org (org-kit:edit-result-updated-document edit)))
```

## Heading and line toggles

Each toggle takes a half-open character span in the original source. A
zero-width span selects the line containing its offset; a nonempty span selects
the lines it intersects. `toggle-heading-comment` adds or removes `COMMENT` on
the headline in the first selected nonblank line. All four functions take a
`document` and `span` argument.

`toggle-heading` chooses a mode from the first nonblank selected line. It
removes heading markers from headline lines, converts list items to headlines,
or turns ordinary lines into headlines one level below the nearest preceding
headline. `toggle-item` removes list markers from item lines, converts
headlines to list items, or prefixes ordinary lines with `- `. Heading-to-item
conversion maps `TODO` and `DONE` to unchecked and checked item checkboxes.
Converting an item to a heading maps a checked checkbox to `DONE` and other
recognized checkboxes to `TODO`.
Only lines matching the selected mode change; blank and directive lines remain
unchanged.

`toggle-fixed-width` adds `: ` to unmarked selected lines. It removes markers
only when every nonblank selected line already has one. Insertion inside
protected Org structures such as source blocks, tables, drawers, list items,
and footnote definitions signals `org-edit-error` with
`:uneditable-structure`.

```lisp
(let* ((document (org-kit:parse-org "plain text"))
       (span (org-kit:make-edit-span 0 0))
       (edit (org-kit:toggle-heading document span)))
  (org-kit:write-org (org-kit:edit-result-updated-document edit)))
```

## Structure templates

`insert-structure-template` wraps a nonempty source span in a block, or inserts
an empty block for a zero-width span. Its `type` accepts the Org 9.8.10 aliases
below or a single-line block specification such as `src emacs-lisp` or
`export html`.

| Alias | Block specification |
| --- | --- |
| `a` | `export ascii` |
| `c` | `center` |
| `C` | `comment` |
| `e` | `example` |
| `E` | `export` |
| `h` | `export html` |
| `l` | `export latex` |
| `q` | `quote` |
| `s` | `src` |
| `v` | `verse` |

For source, example, export, and comment blocks, the function adds a comma
before body lines whose first nonblank content, after any existing commas, is
`*` or starts with `#+`. This includes Org directives such as `#+name:` as well
as block terminators. For example, `#+end_src` becomes `,#+end_src` and
`* Heading` becomes `,* Heading` inside the selected body. The operation
returns an `edit-result` and preserves source outside the selected span.

```lisp
(let* ((source "Some text")
       (document (org-kit:parse-org source))
       (span (org-kit:make-edit-span 0 (length source)))
       (edit (org-kit:insert-structure-template document span "q")))
  (org-kit:write-org (org-kit:edit-result-updated-document edit)))
```

## Timestamps

`adjust-timestamp` changes one date, clock, repeater, or warning component by
one step. Its `delta` is `1` or `-1`. `:endpoint` selects `:start` or, for a
range, `:end`; the default is `:start`, and a single timestamp has no separate
`:end`. Date changes use Org's calendar overflow behavior and update the
weekday. A clock change
changes the selected timestamp's clock; if that timestamp has a clock interval,
both times shift. A date range such as `<...>--<...>` whose selected endpoint
contains a clock interval such as `09:00-10:00` is unsupported for clock
changes. The component must be one of
`:year`, `:month`, `:day`, `:hour`, `:minute`, `:repeater-amount`,
`:repeater-unit`, `:warning-amount`, or `:warning-unit`. Edited years must
remain in the range 0 through 9999.

Repeater and warning amount changes add `delta`, with repeater amounts bounded
below by 1 and warning amounts by 0. Amounts with the `h` unit do not change.
Unit changes step upward as `d` to `w` to `m` to `y`; `h` and `y` stay at their
current unit when stepping upward. Stepping downward changes `y` to `m`, `m` to
`w`, and `w` to `d`; `d` and `h` stay at their current unit. A step at these
bounds is a no-op.

`toggle-timestamp-active` switches active and inactive delimiters around a
complete timestamp or date range, including clock timestamps. Diary timestamps
are unsupported. The edits preserve source outside the timestamp.

```lisp
(let* ((document (org-kit:parse-org "<2024-02-28 Wed>"))
       (timestamp (first-node-of-type document :timestamp))
       (edit (org-kit:adjust-timestamp document timestamp :day 1)))
  (org-kit:write-org (org-kit:edit-result-updated-document edit)))
```

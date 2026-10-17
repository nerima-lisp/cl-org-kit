# Org IDs and stored links

An Org ID is an `ID` property stored directly on a headline or inlinetask; it is
not inherited from a parent heading. `generate-org-id` returns a lowercase UUID
v4 string.
Its optional `:random-source` is a zero-argument function that returns
successive octets from 0 through 255; omitting it creates a fresh random state
with `(make-random-state t)` for each UUID.

## Assign and index IDs

`set-heading-id` assigns a generated ID and returns two values: the updated
document and the ID. It accepts the same optional random source as
`generate-org-id`. An ID value must be non-empty, contain a character other
than spaces or tabs, and have no line break. Before editing, the function
rejects invalid ID values, multiple `ID` properties on one target, and ID values
shared by targets in the same document. Inspect `org-id-error-reason` for the
reason. An existing `ID` on the selected headline is replaced; the generated ID
is rejected if it belongs to another target.

~~~lisp
(let* ((document (org-kit:parse-org (format nil "* Publish notes~%")))
       (heading (find :headline
                      (let ((nodes nil))
                        (org-kit:map-nodes
                         (lambda (node)
                           (when (eq (org-kit:node-type node) :headline)
                             (push node nodes)))
                         (org-kit:document-root document))
                        (nreverse nodes))
                      :key #'org-kit:node-type)))
  (multiple-value-bind (updated id)
      (org-kit:set-heading-id document heading)
    (list id (org-kit:write-org updated))))
~~~

`make-org-id-index` accepts one document or a list of documents and returns an
`org-id-index`. It indexes targets present in each document; a document without
a parse tree contributes no candidates. The index skips empty IDs, IDs made
only of spaces or tabs, and IDs with line breaks. `org-id-index-p` tests whether
a value is an index.
`org-id-index-candidates` returns every matching headline or inlinetask, in the
input document-list order and then target order within each document. Duplicate
IDs on separate targets remain separate candidates. Pass the same document set
to `resolve-link` to resolve `id:` links across documents. See the [link guide](links.md)
for `resolve-link` inputs and result statuses.

## Generate a link from a target

`store-link` returns a complete Org bracket link for a parsed headline or
inlinetask. Choose `:id` (the default), `:custom-id`, or `:heading`:

| Style | Example |
|---|---|
| `:id` | `[[id:123e4567-e89b-42d3-a456-426614174000]]` |
| `:custom-id` | `[[#release-notes]]` |
| `:heading` | `[[*Publish notes]]` |

The `:id` style requires exactly one `ID` property; `:custom-id` requires
exactly one `CUSTOM_ID` property. Each property must remain non-empty after
trimming spaces and tabs and must not contain control characters. Trimming is
used only for validation; `store-link` does not trim the value used in the link.
This check is stricter than the ID index's value check. These styles do not use
another property or a heading title as a fallback. The `:heading` style links
to the target title and uses fuzzy search, so it can be ambiguous when targets
share a title. An ID avoids title-based links, but can still be ambiguous when
multiple documents contain the same value. An invalid target or style signals
`org-store-link-error`; the property styles also signal it for a missing,
repeated, or invalid property, and `:heading` signals it for an empty
title. Inspect `org-store-link-error-reason` and
`org-store-link-error-context` for details.

Given a parsed target with the required property, choose a style with `:style`:

~~~lisp
(org-kit:store-link target :style :custom-id)
~~~

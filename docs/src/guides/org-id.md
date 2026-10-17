# Org IDs and stored links

An Org ID is an `ID` property stored directly on a heading; it is not inherited
from a parent heading. `generate-org-id` returns a lowercase UUID v4 string.
Its optional `:random-source` is a zero-argument function that returns
successive octets from 0 through 255; omitting it uses the current Common Lisp
random state.

## Assign and index IDs

`set-heading-id` assigns a generated ID and returns two values: the updated
document and the ID. It accepts the same optional random source as
`generate-org-id`. An ID value must be non-empty, contain a character other
than spaces or tabs, and have no line break. An invalid ID property or a
duplicate ID property in the same document signals `org-id-error`; inspect
`org-id-error-reason` for the reason. The generated ID also signals that
condition if it already belongs to a different heading in that document.

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

`make-org-id-index` accepts one parsed document or a list of documents and
returns an `org-id-index`. `org-id-index-p` tests whether a value is an index.
`org-id-index-candidates` returns every heading with the requested ID, in the
input document-list order and then heading order within each document. Duplicate
IDs remain separate candidates. Pass the same document set to `resolve-link`
to resolve `id:` links across documents.

## Generate a link from a heading

`store-link` returns a complete Org bracket link for a parsed headline or
inlinetask. Choose `:id` (the default), `:custom-id`, or `:heading`:

| Style | Example |
|---|---|
| `:id` | `[[id:123e4567-e89b-42d3-a456-426614174000]]` |
| `:custom-id` | `[[#release-notes]]` |
| `:heading` | `[[*Publish notes]]` |

The `:id` and `:custom-id` styles each require exactly one property stored on
the selected heading. For `store-link`, the value must remain non-empty after
trimming spaces and tabs and must not contain control characters; this check is
stricter than the ID index's value check. The styles do not fall back to
another. A heading-title link uses fuzzy search and can be ambiguous
when headings share a title; use an ID for a stable cross-document link. An
invalid heading or style, a missing or repeated property, an invalid property
value, or an empty title signals `org-store-link-error`. Inspect its
`org-store-link-error-reason` and `org-store-link-error-context` accessors.

Once `heading` is a parsed headline node, choose the link style explicitly:

~~~lisp
(org-kit:store-link heading :style :custom-id)
~~~

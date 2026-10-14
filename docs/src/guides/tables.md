# Tables

Formatting and structural edits apply to Org pipe tables. Table.el tables are
not supported by these editing functions. Each successful operation returns an
`edit-result`; read its updated document with `edit-result-updated-document`.

## Format, insert, and sort

`table-align` formats cell widths. Row and column indices are one-based.
`table-sort-rows` uses a stable sort; its defaults are ascending lexical order.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(labels ((first-table (document)
           (let ((found nil))
             (org-kit:map-nodes
              (lambda (node)
                (when (and (null found)
                           (eq (org-kit:node-type node) :table))
                  (setf found node)))
              (org-kit:document-root document))
             found)))
  (let* ((document
           (org-kit:parse-org
            (format nil "| Beta | 2 |~%| Alpha | 1 |~%")))
         (aligned
           (org-kit:edit-result-updated-document
            (org-kit:table-align document (first-table document))))
         (inserted
           (org-kit:edit-result-updated-document
            (org-kit:table-insert-row
             aligned (first-table aligned) 3 :values '("Gamma" "3"))))
         (sorted
           (org-kit:edit-result-updated-document
            (org-kit:table-sort-rows
             inserted (first-table inserted) 1 :type :lexical))))
    (org-kit:write-org sorted)))
~~~

The structural edit functions are:

- `table-insert-row` and `table-delete-row`
- `table-move-row`, which moves a row within its hline-delimited section
- `table-insert-column` and `table-delete-column`
- `table-move-column`

Insert functions accept optional cell values as single-line strings.
`table-sort-rows` accepts `:order` as `:ascending` or `:descending`, `:type`
as `:lexical` or `:numeric`, and optional one-based `:start` and `:end` row
positions. The bounds are inclusive and count standard rows; hline rows do not
count. With no bounds, the full standard-row sequence is sorted when the table
has one row section; multiple sections are ambiguous. If either bound is
supplied on a table with hlines, supply both. An explicit range must stay
within one hline-delimited section.

Structural edits preserve unaffected source. Formula remapping accepts one
`TBLFM` directive that the parser can read. Its supported subset includes
assignments whose expressions use numeric literals, cell references, unary or
binary arithmetic, and `vsum`, `vmean`, `vmin`, `vmax`, or `vcount` calls over
ranges. The remapper shifts absolute row and column coordinates in assignment
targets and references; it does not rewrite relative, current-row,
named-column, or hline-anchor coordinates. Other Org formula syntax may be
unsupported. Multiple directives or unsupported syntax fail with
`org-table-error`.

## Errors and limits

Table-specific failures such as an unsupported table type, invalid index, or
ambiguous sort range signal `org-table-error`. General edit validation can
signal `org-edit-error`. If an edit exceeds the document's output,
encoded-output, or node-count ceiling, it signals `org-resource-limit-error`.

Formula token, syntax-depth, numeric-literal precision, and exponent limits
apply when a structural edit parses a supported `TBLFM` directive to remap
references. Numeric sorting also checks decimal precision and exponent. The
formula-work limit applies to internal formula evaluation; version 0.1.0 has no
public formula recalculation operation. See [Resource limits](../reference/node-model.md#resource-limits)
for defaults.

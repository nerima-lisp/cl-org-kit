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

Formula expressions support decimal numeric literals, cell references,
parenthesized expressions, unary `+` and `-`, binary `+`, `-`, `*`, and `/`, and
`vsum`, `vmean`, `vmin`, `vmax`, or `vcount` calls over cell ranges. A range is
valid only as an argument to one of these calls.

Structural edits preserve unaffected source. For formula remapping, one
`TBLFM` directive can be parsed. The remapper shifts absolute row and column
coordinates in assignment targets and references; current-row, relative,
named-column, and hline-anchor coordinates are not rewritten. Other Org formula
syntax may be unsupported. Multiple directives or syntax that cannot be
remapped signal `org-table-error`.

## Recalculate formulas

`table-recalculate` has the signature
`(table-recalculate document target)`. It evaluates a supported `TBLFM`
formula for an Org pipe table and returns an `edit-result`; use
`edit-result-updated-document` to read the result. The input document remains
unchanged. A table with no `TBLFM` is a no-op: the result contains the input
document and no edit spans.

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
            (format nil
                    "| Input | Total |~%|-------+-------|~%| 2 | 0 |~%#+TBLFM: @2$2=$1+1~%")))
         (table (first-table document))
         (edit (org-kit:table-recalculate document table)))
    (org-kit:write-org
     (org-kit:edit-result-updated-document edit))))
~~~

Recalculation supports current-row and relative cell references, plus
named-column references when a header row contains a unique matching name.
Hline-anchored references are unsupported. It accepts only Org pipe tables.
A Table.el table signals `org-table-error`. Multiple `TBLFM` directives or
expressions the formula parser cannot read signal
`org-table-unsupported-syntax`. Semantic formula errors, including invalid or
out-of-range assignment targets or references, unresolved or ambiguous named
columns, unsupported hline-anchored references, and dependency cycles, signal
`org-table-error`. An invalid, foreign, or wrong-type API target can signal
`org-edit-error`. Applicable document or
formula ceilings signal `org-resource-limit-error`.

## Errors and limits

Table-specific failures such as an unsupported table type, invalid index, or
ambiguous sort range signal `org-table-error`. General edit validation can
signal `org-edit-error`. If an edit exceeds the document's output,
encoded-output, or node-count ceiling, it signals `org-resource-limit-error`.

Formula token, syntax-depth, numeric-literal precision, and exponent limits
apply to formula remapping during structural edits and to `table-recalculate`.
Numeric sorting also checks decimal precision and exponent. The formula-work
limit counts cumulative work during one `table-recalculate` call. See
[Resource limits](../reference/node-model.md#resource-limits) for defaults.

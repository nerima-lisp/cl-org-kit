# Tables

Formatting and structural edits apply to Org pipe tables. Table.el tables are
not supported by these editing functions. Each successful operation returns an
`edit-result`; read its updated document with `edit-result-updated-document`.
The examples require the runtime dependencies listed in
[Getting Started](../getting-started.md#asdf) to be available through ASDF.

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

In this call, `1` selects the first table column as the sort key.

The structural edit functions are:

- `(table-insert-row document target index &key values)` and
  `(table-delete-row document target index)`
- `(table-move-row document target from to)`, which moves a row within its
  hline-delimited section
- `(table-insert-column document target index &key values)` and
  `(table-delete-column document target index)`
- `(table-move-column document target from to)`

Structural edit row and column positions are one-based. Row positions count
standard rows across the whole table and ignore hlines. `table-move-row` takes
source and destination positions; a row cannot move across an hline. Without
hlines, all rows are in the same section.

`table-insert-row :values` supplies cells in column order, and
`table-insert-column :values` supplies them in standard-row order. Missing
entries create blank cells. All values are single-line strings.
`table-sort-rows` accepts `:order` as `:ascending` or `:descending`, `:type`
as `:lexical` or `:numeric`, and optional one-based `:start` and `:end` row
positions. The bounds are inclusive and count standard rows; hline rows do not
count. With no bounds, the full standard-row sequence is sorted when the table
has one row section; multiple sections are ambiguous. If either bound is
supplied on a table with hlines, supply both. An explicit range must stay
within one hline-delimited section.

Formula expressions support decimal arithmetic, comparisons, duration and
string literals, field and range references, parentheses, and the built-in
functions listed below. A range can be used as a vector-function argument or
as an assignment target.

Structural edits preserve source outside the changed table rows or cells and
any `TBLFM` formula segments that need remapping. For formula remapping, one
`TBLFM` directive can be parsed. The remapper shifts absolute row and column
coordinates in assignment targets and references; current-row, relative,
named-column, and hline-anchor references are left as written, even if an edit
changes which cells they resolve to. Other Org formula
syntax may be unsupported. Multiple directives or syntax that cannot be
remapped signal `org-table-error`. This structural-edit limit is separate from
recalculation, which follows Org's first-directive rule below.

## Recalculate formulas

`table-recalculate` has the signature
`(table-recalculate document target &key functions)`. `target` must resolve to
an Org pipe-table node in `document`. It evaluates formulas for that table and
returns an `edit-result`; use
`edit-result-updated-document` to read the result. The input document remains
unchanged. With no `TBLFM`, the result contains the input document and no edit
spans. If several `#+TBLFM` directives follow a table, recalculation applies
only the first, as in Org. Assignments within that directive are separated by
`::`.

Row and column coordinates are one-based. Row numbers count standard rows,
including the header, and ignore hline rows. `@I`, `@II`, and later Roman
numerals select the first standard row after the matching hline. `@-I` and
`@+I` select the row after the nearest hline above or below the current row.
For example, `@-I$2` selects column 2 in the row below the hline above.
`@I..II` spans from the row after the first hline through the row before the
second; omitting the second `@` is Org shorthand for `@I..@II`. `@III+1`
selects the first standard row after the third hline, and `@III+2` selects the
second standard row after the third hline. Other supported references include
absolute rows such as `@3$2`, the current row such as `@0$2`, a relative row
such as `@+1$2`, the first and last rows (`@<`, `@>`), and named columns such
as `@>$Price`. Named columns use the table's header row.

Assignments can target a field, a row or rectangular range, or a whole column.
A row formula such as `@3=...` assigns across the selected standard row. A
rectangular range formula such as `@2$2..@4$3=...` assigns across that range. A
column formula such as `$3=...` applies to the data rows. When a table has
hlines, rows before its first hline are treated as the header section and are
skipped by column formulas. A field, row, or range formula takes precedence
over a column formula for any field both formulas target, but the column formula
still runs and can signal an error. If two field, row, or range formulas target
the same field, recalculation signals `org-table-unsupported-syntax` with
feature `:duplicate-target`.

An expression can read a field or range from another named table in the same
document with `remote(name, ref)`. Give the source table a `#+NAME:` immediately
before it; `name` can be bare or quoted, and `ref` is an absolute field or
range reference in that table. Names are matched without regard to case. A
remote read uses the source table's saved cell contents and does not recalculate
its `TBLFM`. A remote range returns its cells in row-major order and can be
passed to a vector function, for example
`vsum(remote(inputs,@2$1..@3$1))`. Remote references to another document are
not supported.

`#+CONSTANTS:` defines document-level formula constants, for example
`#+CONSTANTS: pi=3.14`; reference that constant as `$pi` in a formula. A
property can be referenced as `$PROP_name`; lookup starts at the table's
containing heading and continues through its ancestors.

### Built-in Calc functions

These examples use the Org Calc formula syntax. For the vector examples, assume
the current row contains `2`, `4`, and `6` in columns 1 through 3.

| Function | Example | Calculation |
| --- | --- | --- |
| `exp` | `exp(1)` | `2.7182818` |
| `sin` | `sin(30);D` | `0.5` |
| `tan` | `tan(45);D` | `1.` |
| `vmean` | `vmean($1..$3)` | `4` |
| `vsum` | `vsum($1..$3)` | `12` |
| `taylor` | `taylor(exp(x),x=7,2)` | `1096.63315843 x + 548.316579215 (x - 7)^2 - 6579.79895058` |
| `if` | `if(3,2,1)` | `2` |
| `string` | `string(42)` | The text `42` |
| `typeof` | `typeof("a")` | Type code `101` |

The current `taylor` implementation accepts `taylor(exp(x),x=point,2)`, where
`point` is a numeric expression, and returns a second-order expansion around
that value.

The existing vector helpers `vmin`, `vmax`, and `vcount` are also available.

`if` accepts boolean expressions and numeric conditions; zero is false and any
nonzero number is true. For example, `if(0,2,1)` returns `1`. Calc type code `1`
denotes an integer, while `101` denotes a string.

Other function names can be supplied for one recalculation call. Built-in names
take precedence and cannot be overridden. `:functions` is an alist from
function-name strings to Common Lisp function designators.
Callbacks receive evaluated positional arguments as numbers or strings and
must return a real number or string. An unregistered function signals
`org-table-unsupported-syntax` with feature `:unknown-function`.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(defun triple-value (number)
  (* number 3))

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
                    "| Input | Triple |~%|-------+--------|~%| 3 | 0 |~%#+TBLFM: @2$2=triple($1)~%")))
         (table (first-table document))
         (edit
           (org-kit:table-recalculate
            document table :functions '(("triple" . triple-value)))))
    (org-kit:write-org
     (org-kit:edit-result-updated-document edit))))
~~~

### Formula mode flags and durations

Add flags after a semicolon in a formula. Supported flags are `pN`,
`nN`/`sN`/`eN`/`fN`, `D`/`R`, `F`/`S`, `u`, `T`/`t`/`U`, and `E`/`N`.
`pN` sets calculation precision, bounded by the document's
`max-formula-precision` limit; this is separate from the default result display
precision, so `exp(1);p20` still displays `2.7182818`. `nN`, `sN`, and `eN`
format numeric results as normal, scientific, or engineering output with the
selected significant digits. `fN` formats a fixed number of decimal places;
the special `f-1` format emits blank output for `nan` and uses one decimal place
for other numbers. For example, `1/3;n3` gives `0.333`, `12345.0;s3` gives
`1.23e4`, `12345.0;e2` gives `12e3`, and `1/3;f4` gives `0.3333`. `D` selects
degrees and is the default angle mode; `R` selects radians for `sin` and `tan`.
Upper-case `E` and `N` control empty and non-numeric fields; lower-case `eN`
and `nN` select number formats. `E` interprets empty fields as `nan`. `N` treats
empty and non-numeric fields as zero, including when `E` is also present. For
example, `vmean($1..$7);EN` combines the two field modes. `F` formats numeric
results as Calc fractions, so `1/3;F` produces `1:3`. `S` emits ordinary
numeric output for exact values supported by decimal arithmetic; symbolic
algebra is not implemented. `u` leaves unit-free numeric results unchanged;
unit literals and conversion are not implemented. Unsupported symbolic or
unit-bearing operations signal `org-table-error` with reason
`:unsupported-mode`. `T`, `t`, and `U` enable duration handling as described
below. `L` selects Lisp formula mode, which this implementation does not
support.

Formula literals accept `HH:MM` or `HH:MM:SS` and represent seconds. Referenced
table cells are read as durations when the assignment has `T`, `t`, or `U`;
minutes and seconds must be less than 60, and table-cell minute and second
fields must each use two digits. The hour field can have one or more digits.
`T` formats a result as `HH:MM:SS`, `U` as `HH:MM`, and
`t` as decimal hours rounded to two places. If columns 1 and 2 contain `2:12`
and `1:47`, `$1+$2;T` returns `03:59:00` and `$1+$2;U` returns `03:59`. With
`3:02:20` and `-2:07:00` in those cells, `$1+$2;t` returns `0.92`. A leading
minus sign represents a negative duration, and integer operands represent
seconds.

See the Org manual for [references](https://orgmode.org/manual/References.html),
[field and range formulas](https://orgmode.org/manual/Field-and-range-formulas.html),
[column formulas](https://orgmode.org/manual/Column-formulas.html),
[Calc formula syntax](https://orgmode.org/manual/Formula-syntax-for-Calc.html),
[durations and time values](https://orgmode.org/manual/Durations-and-time-values.html),
and [editing and debugging formulas](https://orgmode.org/manual/Editing-and-debugging-formulas.html).

Recalculation supports Org pipe tables only. A Table.el table signals
`org-table-error`. Formula syntax the parser cannot read signals
`org-table-unsupported-syntax`. Semantic errors, including invalid or
out-of-range targets or references, unresolved or ambiguous named columns, and
formula dependency cycles, signal `org-table-error`. An invalid, foreign, or
wrong-type API target can signal `org-edit-error`. Applicable document or
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

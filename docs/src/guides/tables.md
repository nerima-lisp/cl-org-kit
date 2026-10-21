# Tables

Formatting and structural-edit functions apply to Org pipe tables. Table.el
tables are not supported by these functions. Each operation that edits source
returns an `edit-result`; read its updated document with
`edit-result-updated-document`.
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
    (write-string (org-kit:write-org sorted))))
~~~

~~~text
| Alpha | 1 |
| Beta  | 2 |
| Gamma | 3 |
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

Structural edit row and column positions are one-based. Insertions go before
the row or column at the given index; an index one past the last position
appends. Row positions count standard rows across the whole table and ignore
hlines. If an insertion index selects the first standard row after an hline,
the new row is inserted in that section. `table-move-row` takes source and
destination positions; a row cannot move across an hline.

`table-insert-row :values` supplies cells in column order, and
`table-insert-column :values` supplies them in standard-row order. Missing
entries create blank cells. Supplying more entries than the table has columns
or standard rows signals `org-table-error`. All values are single-line strings.
`table-sort-rows` accepts `:order` as `:ascending` or `:descending`, `:type`
as `:lexical` or `:numeric`, and optional one-based `:start` and `:end` row
positions. The bounds are inclusive and count standard rows; hline rows do not
count. With no bounds, the full standard-row sequence is sorted when the table
has one row section; multiple sections are ambiguous. If either bound is
supplied on a table with hlines, supply both. An explicit range must stay
within one hline-delimited section.

Expressions accepted by `table-recalculate` support decimal arithmetic, comparisons, duration and
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
`(table-recalculate document target &key functions callbacks)`. `target` must
resolve to an Org pipe-table node in `document`. It evaluates formulas for that
table and returns an `edit-result`; use
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
as `@>$Price`. The first standard row before the first hline is the header
row for named columns. Without an hline, the table has no header row.

Assignments can target a field, a row or rectangular range, or a whole column.
A row formula such as `@3=...` assigns across the selected standard row. A
rectangular range formula such as `@2$2..@4$3=...` assigns across that range. A
column formula such as `$3=...` applies to the data rows. When a table has
hlines, rows before its first hline are treated as the header section and are
skipped by column formulas. Column formulas run first; field, row, and range
formulas then read their results and replace them for any field both formulas
target. The column formula still runs and can signal an error. If two field,
row, or range formulas target
the same field, recalculation signals `org-table-error` with reason
`:duplicate-target`.

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
property can be referenced as `$PROP_name`, where `name` is the property name;
lookup starts at the table's containing heading and continues through its
ancestors.

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
| `typeof` | `typeof("a")` | Type code `101` (string) |

The `taylor` built-in accepts `taylor(exp(x),x=point,2)`, where `point` is a
numeric expression, and returns a formatted second-order polynomial around
that value. It is a specific supported case; general symbolic algebra is not
implemented.

The existing vector helpers `vmin`, `vmax`, and `vcount` are also available.

`if` accepts boolean expressions and numeric conditions; zero is false and any
nonzero number is true. `typeof` returns type code `1` for integers and `101`
for strings.

Other function names can be supplied for one recalculation call. Built-in names
take precedence and cannot be overridden. `:functions` is an alist from
function-name strings to Common Lisp function designators. These callbacks
receive evaluated positional arguments as numbers or strings and must return a
real number or string. An unregistered function signals `org-table-error` with
reason `:unknown-function`.

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
    (write-string
     (org-kit:write-org
      (org-kit:edit-result-updated-document edit)))))
~~~

~~~text
| Input | Triple |
|-------+--------|
| 3 | 9 |
#+TBLFM: @2$2=triple($1)
~~~

### Formula mode flags and durations

Add flags after a semicolon in a formula. Supported flags are `pN`,
`nN`/`sN`/`eN`/`fN`, `D`/`R`, `F`/`S`, `u`, `T`/`t`/`U`, `E`/`N`, and `L`.
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
`:unsupported-mode`. `T`, `t`, and `U` enable duration handling. `L` selects Lisp
reference insertion for quoted Lisp expressions; the evaluator callback
behavior is described below.

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
formula dependency cycles, signal `org-table-error`. An invalid API target, a
target from another document, or a target of the wrong type can signal
`org-edit-error`. Applicable document or formula ceilings signal
`org-resource-limit-error`.

### Lisp formula callbacks

The `;L` flag selects Lisp reference insertion. A formula expression beginning
with a quoted list, such as `'(identity $1)`, is sent to the evaluator callback
registered in a caller-owned `callback-registry` passed as `:callbacks`.
See [Callbacks and Write Plans](callbacks-and-write-plans.md#register-callbacks-explicitly)
for a registration example.
Without `;L`, Org-style cell references are expanded and quoted as strings
before the expression is passed to the callback. With `;L`, references remain
literal, so the callback receives `'(identity $1)`. The library never evaluates
Emacs Lisp. A missing evaluator signals `org-elisp-evaluator-error` with reason
`:unregistered`; evaluator results must be strings or real numbers.

### Calling Babel blocks from formulas

A quoted Lisp formula can call a named source block with `org-sbe`. Pass the
table recalculation `:callbacks` registry with an executor registered for the
block's language. The executor receives the expanded body, resolved header
parameters, and resolved variables; the library does not start a process.
Recalculation recognizes `org-sbe` directly and routes it to the Babel
executor; it does not call the quoted-Lisp formula evaluator for this form.

```org
#+NAME: subtotal
#+BEGIN_SRC demo-language
body passed to the registered executor
#+END_SRC

| Input | Result |
|-------+--------|
| old   | old    |
#+TBLFM: @2$2='(org-sbe subtotal ":session one" (limit $ "4"))
```

This complete example registers the executor, recalculates the table, and
prints its updated document:

```lisp
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
  (let* ((source
           (format nil
                   "#+NAME: subtotal~%#+BEGIN_SRC demo-language~%body~%#+END_SRC~%| Input | Result |~%|-------+--------|~%| old   | old    |~%#+TBLFM: @2$2='(org-sbe subtotal \":session one\" (limit $ \"4\"))~%"))
         (document (org-kit:parse-org source))
         (table (first-table document))
         (callbacks (org-kit:make-callback-registry)))
    (org-kit:register-babel-executor
     callbacks "demo-language"
     (lambda (body parameters variables)
       (declare (ignore body))
       (format nil "session=~A; limit=~A"
               (cdr (assoc "SESSION" parameters :test #'string=))
               (cdr (assoc "limit" variables :test #'string=)))))
    (write-string
     (org-kit:write-org
      (org-kit:edit-result-updated-document
       (org-kit:table-recalculate document table :callbacks callbacks))))))
```

Resolved parameter keys are uppercase strings such as `"SESSION"`. The
`org-sbe` variable key keeps the spelling used in the formula, so the executor
looks up `"limit"` as written. The result cell contains
`session=one; limit=4`.

The block name can be a symbol or string. The optional second string supplies
header arguments, as `":session one"` does above. It is followed by zero or
more `(variable value...)` groups. The values in each group become one variable
value; multiple values form a list. The `$` marker makes the next value a
literal string. Org's `org-sbe` behavior returns an empty string for an empty
block name and trims whitespace from the executor result. A non-string result
is formatted as a Lisp value. If the block's language has no registered
executor, recalculation signals `org-elisp-evaluator-error` without calling
the Emacs Lisp evaluator.

## Plot write plans

`table-plot-write-plan` reads the table's `#+PLOT` options and returns a
`write-plan` for the script and data. It supports the `title`, `ind`, `deps`,
`type`, `with`, `file`, `set`, `labels`, and `script` options. Pass safe relative
paths with `:script-path` and `:data-path`; radar plots also require
`:radar-path` for their setup data. If the `script` option refers to existing
script content, supply it with `:script-content`; each `$datafile` placeholder
in that content is replaced with `:data-path`. `file` names the image output
inside the generated script. The generated script is executable gnuplot code
built from the document's options and any supplied script content. It may run
commands from those sources, including gnuplot's `system` function. Pass it to
gnuplot only when the document and any supplied script content are trusted.
The library returns a write plan; it does not create files or execute gnuplot.
The image output named by `file` may be an absolute path or contain `..`, so it
is not confined to the write-plan root. Command-valued options reject line
feeds, carriage returns, and NUL characters.
Inspect and apply the returned plan as described in [Build and inspect a write
plan](callbacks-and-write-plans.md#build-and-inspect-a-write-plan). Unknown
options signal `org-table-error` naming the option.

## CSV and TSV conversion

`table-import` has the signature
`(table-import document offset text &key format)` and inserts delimited text at
the zero-based character `offset` as an aligned Org pipe table. `format` accepts
`:csv`, `:tsv`, or `:auto`; automatic detection chooses TSV when it finds a tab
outside quoted text and CSV otherwise. It returns an `edit-result` and preserves
source outside the insertion. CSV supports quoted fields, including quoted
delimiters and doubled quotes. In TSV, quote characters are ordinary text.
Multiline fields are rejected.

`table-export` returns text without writing a file. Its format accepts `:csv`,
`:tsv`, or `:auto`; the default and `:auto` format are TSV. CSV fields with a
comma or quote are quoted, and embedded quotes are doubled. Hlines are omitted,
as in Org export.

## Radio tables

`table-send-radio` transforms a table with one or more consecutive
`#+ORGTBL: SEND` directives on the lines immediately before it. For each SEND
name, it replaces the body between matching `BEGIN RECEIVE ORGTBL` and
`END RECEIVE ORGTBL` markers; names match without regard to case. It returns an
`edit-result`; text outside the receive bodies is preserved. The supported
formats are `orgtbl`, `generic`, `csv`, `tsv`, `latex`, `html`, and `texinfo`.
SEND parameters accept these common keys: `:sep`, `:hsep`, `:hline`, `:splice`,
`:skip`, `:tstart`, `:tend`, `:lstart`, `:lend`, `:llstart`, `:llend`, `:hlstart`,
`:hlend`, `:hllstart`, `:hllend`, `:fmt`, `:hfmt`, `:lfmt`, `:llfmt`, `:hlfmt`,
`:hllfmt`, `:backend`, and `:raw`. The `latex` transformation also accepts
`:booktabs` and `:environment`; `texinfo` also accepts `:columns`. Values must
be strings, integers, `t`, or `nil`; executable and list-valued parameters are
rejected. `:raw` is accepted but currently has no effect. `:fmt` and `:hfmt`
templates apply to `generic` without `:backend`, `csv`, and `tsv`; the other
transformations ignore them. For example,
`#+ORGTBL: SEND summary generic :sep "," :skip 1` uses commas between cells and
skips the first input row. The `:fmt` and `:hfmt` string templates accept `%s`
placeholders for data and header cells in those transformations.

## Transpose

`table-transpose` transposes a rectangular Org pipe table and returns an
`edit-result`. Hlines are removed, `TBLFM` text is preserved unchanged, and
text outside the table remains intact. Formula references are not remapped, so
review or rewrite them before recalculation if they should follow the moved
cells. Ragged tables and Table.el tables signal `org-table-error`.

## Errors and limits

Table-specific failures such as an unsupported table type, invalid index,
ambiguous sort range, ragged transpose, or unsupported plot option signal
`org-table-error`.
General edit validation can signal `org-edit-error` for an invalid target, a
node from another document, or a target of the wrong type. If an edit or
write-plan exceeds a document's input, output, encoded-output, node-count, or
write-plan ceiling, it signals `org-resource-limit-error`.

Formula token, syntax-depth, numeric-literal precision, and exponent limits
apply to formula remapping during structural edits and to `table-recalculate`.
Numeric sorting also checks decimal precision and exponent. The formula-work
limit counts cumulative work during one `table-recalculate` call. See
[Resource limits](../reference/node-model.md#resource-limits) for defaults.

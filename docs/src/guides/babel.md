# Babel

The Babel APIs resolve headers and variables, expand noweb references, execute
blocks through registered executors, return result updates as edit results, and
build tangle write plans. They do not start external processes or apply tangle
plans; callers choose the executors and when to apply plans.

## Example document and source block

```lisp
(defparameter *babel-document*
  (org-kit:parse-org
   "#+NAME: sample-items
- red
- blue

#+NAME: subtotal
#+BEGIN_SRC emacs-lisp
42
#+END_SRC

#+NAME: subtotal-call
#+CALL: subtotal()

#+NAME: static-consumer
#+BEGIN_SRC emacs-lisp :var items=sample-items
(print items)
#+END_SRC

#+NAME: consumer
#+BEGIN_SRC emacs-lisp :var total=subtotal-call :noweb yes
(print total)
<<subtotal()>>
#+END_SRC"))

(defun named-src-block (document name)
  (let ((found nil))
    (org-kit:map-nodes
     (lambda (node)
       (when (and (eq (org-kit:node-type node) :src-block)
                  (equal (org-kit:node-property node :name) name))
         (setf found node)))
     (org-kit:document-root document))
    found))

(defparameter *static-consumer-block*
  (named-src-block *babel-document* "static-consumer"))

(defparameter *consumer-block*
  (named-src-block *babel-document* "consumer"))

(defparameter *subtotal-block*
  (named-src-block *babel-document* "subtotal"))

(defun first-node-of-type (document type)
  (let ((found nil))
    (org-kit:map-nodes
     (lambda (node)
       (when (and (null found) (eq (org-kit:node-type node) type))
         (setf found node)))
     (org-kit:document-root document))
    found))

(defparameter *subtotal-call*
  (first-node-of-type *babel-document* :babel-call))
```

## Header arguments

Pass `org-kit:resolve-babel-parameters` a document and one of its `:src-block` nodes.
It returns an alist of key-value pairs.
Keys are uppercase strings.
Values are strings, or `nil` for flags.

The effective order is defaults, generic `header-args` properties,
language-specific properties, affiliated `#+HEADER` arguments, and source-block
arguments. A later value replaces an earlier value.
The built-in values are `:session none`, `:results replace`, `:exports code`, `:cache no`, `:noweb no`, `:hlines no`, and `:tangle no`.
Heading inheritance is resolved within each property layer before the next
layer is applied, so an inherited language-specific value overrides an
inherited generic value.
The `+` suffix appends a property value to its layer.
`:var` assignments merge by variable name.
`:results` options have four independent groups:

| Group | Values |
|---|---|
| Format | `file`, `list`, `vector`, `table`, `scalar`, `verbatim` |
| Representation | `raw`, `html`, `latex`, `org`, `code`, `pp`, `drawer`, `link`, `graphics` |
| Handling | `replace`, `silent`, `none`, `discard`, `append`, `prepend` |
| Result channel | `output`, `value` |

Choose at most one value from each group. A later value replaces only an earlier value from the same group.
Header resolution preserves this option set. `update-babel-results` supports a
defined subset of formats and operations; it signals an error for parsed options
that its writer does not support.
For HTML, GFM, and ASCII export, `:exports` controls the source block and its results:

| `:exports` value | Rendered content |
|---|---|
| `code` | Source block |
| `results` | Results |
| `both` | Source block and results |
| `none` | Neither |

```org
* Parent
:PROPERTIES:
:header-args: :session parent-generic
:header-args:emacs-lisp: :session parent-language
:END:
** Child
:PROPERTIES:
:header-args:emacs-lisp+: :var child=1
:END:
#+NAME: inherited-block
#+BEGIN_SRC emacs-lisp
body
#+END_SRC
```

Bind the document and its named source block before resolving parameters:

```lisp
(defparameter *inheritance-document*
  (org-kit:parse-org
   (format nil
           "* Parent~%:PROPERTIES:~%:header-args: :session parent-generic~%:header-args:emacs-lisp: :session parent-language~%:END:~%** Child~%:PROPERTIES:~%:header-args:emacs-lisp+: :var child=1~%:END:~%#+NAME: inherited-block~%#+BEGIN_SRC emacs-lisp~%body~%#+END_SRC~%")))

(defparameter *inherited-block*
  (named-src-block *inheritance-document* "inherited-block"))

(org-kit:resolve-babel-parameters
 *inheritance-document* *inherited-block*)
```

The result resolves `SESSION` to `parent-language` and includes the `child=1`
variable.

```lisp
(org-kit:resolve-babel-parameters *babel-document* *static-consumer-block*)
```

The call returns raw header values.
Use `org-kit:resolve-babel-variables` to parse the block's `:var` assignments.

## Static `:var` values

`org-kit:resolve-babel-variables` returns an alist from variable names to Common Lisp values.
Numeric literals are signed decimal integers or decimal floating-point values with an optional `e` exponent, such as `42`, `-.25`, and `1.25e+2`; ratios such as `3/4` remain strings.
It also accepts double-quoted strings, named Org pipe tables, and named Org plain lists.
Numeric table cells become numbers.
Other unquoted table cells and list items remain strings.
A named Org plain list can be used when each item is empty or has exactly one paragraph child.

```lisp
(org-kit:resolve-babel-variables *babel-document* *static-consumer-block*)
;; => (("items" . ("red" "blue")))
```

A bare value such as `sample-items` names another Org object; a missing name signals `org-kit:org-babel-error`.
Reader syntax such as `(+ 1 2)` is rejected instead of being evaluated.
The library never evaluates a `:var` expression.
Use `:result-resolver` for executable references.
The callback receives `(block-name resolved-variable-alist)` and returns that block's result as a Common Lisp value.
It may return `nil`; signal an error from the callback when its lookup has no result.
```lisp
(defparameter *executed-results* (make-hash-table :test #'equal))
(setf (gethash "subtotal" *executed-results*) 42)

(defun lookup-babel-result (block-name variables)
  (declare (ignore variables))
  (multiple-value-bind (value found-p)
      (gethash block-name *executed-results*)
    (unless found-p
      (error "No result for Babel block ~A" block-name))
    value))

(org-kit:resolve-babel-variables
 *babel-document* *consumer-block*
 :result-resolver #'lookup-babel-result)
```

For the document above, this returns `(("total" . 42))`.

In the example, `#+NAME: subtotal-call` labels `#+CALL: subtotal()` for `:var total=subtotal-call`.
The callback receives `subtotal` as the source-block name.
Without a callback, an executable result reference signals `org-kit:org-babel-error`.
Its reason is `:missing-result-resolver`.
The `:max-babel-result-elements` limit bounds callback results resolved by both
`org-kit:resolve-babel-variables` and `org-kit:expand-babel-noweb`.
It defaults to 1,000,000 visited objects per callback result; containers and each list cell count as objects.
Pass `:limits (org-kit:make-limits :max-babel-result-elements N)` to `org-kit:parse-org` to set a different value.

`#+NAME: name` or `#+RESULTS: name` labels a static Org pipe table or plain list in the same document for `:var` lookup without a callback; use `name` as the `:var` value.
A table's rule rows are omitted.
Other named node types, such as paragraphs and headlines, and tables formatted by Emacs's `table.el` package rather than as Org pipe tables signal `org-kit:org-babel-error`.
List items with nested content, multiple children, or a non-paragraph child signal `org-kit:org-babel-error`.

## Noweb expansion

Pass `org-kit:expand-babel-noweb` a document, one of its source blocks, and a context: `:eval`, `:tangle`, or `:export`.
Use `:eval` when preparing code for evaluation, `:tangle` when preparing code to tangle to a file, and `:export` when exporting.
It returns the block body without its final line ending.
`:noweb` selects when references expand:

| `:noweb` value | `:eval` | `:tangle` | `:export` |
|---|---|---|---|
| `yes` | Expand | Expand | Expand |
| `no` | Keep references | Keep references | Keep references |
| `eval` | Expand | Keep references | Keep references |
| `tangle` | Keep references | Expand | Keep references |
| `strip-export` | Expand | Expand | Remove reference markers |
| `no-export` | Expand | Expand | Keep references |
| `strip-tangle` | Expand | Remove reference markers | Expand |

For `strip-export` and `strip-tangle`, the reference marker is removed and the referenced body is not inserted.

For `<<name>>`, a source block named `name` takes precedence and its body is inserted.
If there is no such block, `<<name>>` expands the group of blocks with `:noweb-ref name` in document order.
Before each group member after the first, expansion inserts `:noweb-sep` from the previous member.
The default separator is a newline.
An unknown plain reference expands to an empty string.
Referenced blocks expand recursively only when their own `:noweb` mode allows
expansion in that context. Otherwise their body is inserted with any noweb
markers it contains left unchanged.

`<<name()>>` requests a block result through `:result-resolver`.
Pass the same callback used by `org-kit:resolve-babel-variables` with the `:result-resolver` keyword to `org-kit:expand-babel-noweb`:

```lisp
(org-kit:expand-babel-noweb
 *babel-document* *consumer-block* :context :eval
 :result-resolver #'lookup-babel-result)
```

With the document above, this returns `(print total)` followed by `42`.

String results are inserted as written.
Numbers, characters, symbols, conses, and vectors use Common Lisp printed form.

By default, all text before `<<name>>` on its source line prefixes the first inserted line and repeats on later inserted lines.
For example, `prefix: <<name>>` with `first` and `second` on separate lines becomes `prefix: first` and `prefix: second`.
Use `:noweb-prefix no` to disable this behavior.
Cycles signal `org-kit:org-babel-error`.
Depth and cumulative expanded-character ceilings signal `org-kit:org-resource-limit-error`.

See [Resource limits](../reference/node-model.md#resource-limits) for the Babel ceilings.
See [API stability](../reference/api-stability.md) for the public API policy.

## Executing source blocks and calls

`org-kit:register-babel-executor` registers an executor in a caller-owned
`org-kit:callback-registry` under the `:babel-executor` kind and a language
name. No executor is registered by default. The executor receives the expanded
body, the resolved header-parameter alist, and the resolved variable alist.
Parameter keys are uppercase strings, and `"SESSION"` carries the session
identifier. The executor owns any state associated with that identifier.

```lisp
(defparameter *babel-executors* (org-kit:make-callback-registry))

(org-kit:register-babel-executor
 *babel-executors* "emacs-lisp"
 (lambda (body parameters variables)
   (declare (ignore parameters variables))
   body))

(org-kit:execute-babel-source-block
 *babel-document* *subtotal-block* *babel-executors*)
```

An executor for a real language would run or otherwise process `body` according
to the caller's policy.

### Reference executors

#### Common Lisp executor

`org-kit:make-babel-common-lisp-reference-executor` returns an opt-in executor
that evaluates source-block code in the current Lisp process. Register it only
for trusted documents. A custom executor may also evaluate or pass block bodies
to another program; its caller decides which documents it may process.

#### Shell executor

`org-kit:make-babel-shell-reference-executor` returns an opt-in executor that
assembles a shell script and delegates execution to a caller-supplied runner.
Its signature is `(org-kit:make-babel-shell-reference-executor runner &key (shell "sh"))`.
The helper never starts a process.
Register the returned executor for the `shell` language:

```lisp
(org-kit:register-babel-executor
 *babel-executors* "shell"
 (org-kit:make-babel-shell-reference-executor #'run-shell))
```

Define `run-shell` to accept `(shell script stdin cmdline)` and return stdout as
a string. `shell` is the configured shell name. When `:stdin`, `:cmdline`, or a
non-whitespace `:shebang` selects script mode (described below), `script` is
the complete assembled script; otherwise, it is the trimmed expanded body.
`stdin` and `cmdline` are `nil` when absent and strings when present. The helper
passes `cmdline` unchanged. It does not tokenize, split, quote, or validate that
string; the runner chooses how its process API maps it and how to provide
`script` and `stdin` to the selected shell.

#### Variable assignments and stdin

The helper expands `:prologue`, one assignment for each Babel `:var` binding,
the body, and `:epilogue` in that order. A configured shell name ending in the
case-sensitive suffix `bash` gets Bash-specific array forms for flat lists and
tables whose first row has at least two cells. The names `bash` and
`/usr/bin/bash` qualify. A flat `:var` list such as `("red" "blue")` becomes
an indexed array; a table such as `(("one" "1") ("two" "2"))` becomes an
associative array. Each row's first cell is its key; the remaining cells are
serialized as its value and joined with newlines when there is more than one.
Uneven row widths are not rejected; a row without a value stores an empty
value. If a nonempty proper list's first nested row has fewer than two cells,
the list uses the indexed-array form instead. A one-cell row such as `("red")`
becomes the item `red`; a wider row joins its cells with newlines.
Bash scalar bindings use the generic `NAME='value'` assignment. The
helper generates lists with `unset NAME` followed by `declare -a NAME=( ... )`;
tables use `unset NAME`, then `declare -A NAME`, followed by one
`NAME['key']='value'` line per row.

Fish and generic assignments treat a list whose first item is a row or hline
as a table; other proper lists become newline-separated values. A shell name
ending in the case-sensitive suffix `fish` uses `set NAME 'value'`. Shell
names without the `bash` or `fish` suffix, including the default `sh`, use
generic assignment text; the helper does not check or guarantee that another
shell accepts its assignment or quoting syntax. In the generic and fish
forms, flat lists become newline-separated text and tables become
separator-delimited rows. Each value is one quoted assignment value; a
fish list therefore contains embedded newlines in one argument. Bash array
forms quote each item separately. These rules describe assignment generation
only; the helper does not validate that the body syntax matches the configured
shell. Generic and fish assignments use the same single-quoted argument form;
in every generated assignment, an embedded single quote is emitted as
`'"'"'`. Fish concatenates those adjacent quoted segments into one argument,
so the generated form preserves a literal single quote there too. For example,
the value `can't` produces `NAME='can'"'"'t'`. Shell expansions such as
`$HOME` remain literal.

For `:stdin`, strings remain unchanged, lists become newline-separated text,
and tables use the `:separator` value between cells and newlines between rows.
A scalar `:stdin` value, list element, and table cell use these forms: `NIL`
becomes `nil`, symbols use lowercase names, numbers and characters use their
printed text, and other values use lowercase readable text. `:separator`
defaults to a tab and applies when a table is serialized as text, including
`:stdin` and non-Bash assignments. An Org hline is a horizontal separator row
in an input table. For Babel `:var` and `:stdin` tables serialized as text,
hlines are omitted unless `:hlines yes` is set; then `:hline-string` supplies
their text, defaulting to `hline`.

#### Script assembly

Any non-nil `:stdin` or `:cmdline`, or a `:shebang` containing a non-whitespace
character, selects script mode. When script mode is active, a supplied
`:shebang` is copied verbatim and then the helper appends an LF after it; the
supplied text, including embedded or trailing LFs, is preserved. The helper
does not add a `#!` prefix to a supplied shebang. If `:shebang` is absent, it
uses `#!/usr/bin/env <shell>`. An empty `:shebang` combined with `:stdin` or
`:cmdline` produces an empty first line. A whitespace-only shebang does not
select script mode by itself; if `:stdin` or `:cmdline` selects it, that text is
copied as supplied. By default, `:padline` adds one more LF before the body.
With a shebang that does not end in LF, this creates one blank line; with
exactly one trailing LF, it creates two. Any further trailing LFs remain as
supplied. `:padline no` omits only this extra padding LF. Without script mode,
the helper trims whitespace around the expanded body and passes it without a
shebang or padding.

#### Output results

For shell results, Org 9.8.10 is the compatibility reference, not a runtime
requirement. It has no `org-babel-expand-body:shell`; `org-babel-execute:shell`
passes `org-babel-variable-assignments:shell` output to
`org-babel-expand-body:generic`, which inserts the prologue, assignments, body,
and epilogue in that order.

When `:results` omits a channel, shell blocks default to `output`.
`:results output` returns runner stdout unchanged. `:results output table`
imports stdout through Org's `org-babel-import-elisp-from-file` path, which
calls `org-table-convert-region` with `babel-auto` separator detection. Exact
empty lines are ignored when selecting a separator, but whitespace-only lines
are not. TAB is selected when every nonempty line contains a tab; otherwise,
CSV is selected when every nonempty line contains a comma. TAB takes
precedence when lines contain both. If neither applies, multiple lines use
whitespace; a single line with neither TAB nor comma has no inferred
separator. Non-final exact empty rows remain in the imported data when another
line selects TAB or CSV. In whitespace mode, a whitespace-only line becomes a
row containing an empty cell before ragged rows are padded.

After separator conversion, pipe characters are interpreted as Org table
syntax even when a line also contains the selected separator. A row `|a|b|`
imports as `("" "a" "b")`; two tight rows,
`|a|b|` and `|c|d|`, import as `(("" "a" "b") ("" "c" "d"))`. A spaced
row such as `| a | b |` imports as seven cells, with empty cells around the
values. Ragged rows are padded to the widest row, and a one-row, one-cell table
collapses to its cell. The helper has no hline-removal pass: `|---+---|`
remains data, as does a row containing `---` and `ignored` separated by a tab.
Cell handling trims surrounding whitespace and strips outer double quotes
literally, including `""`; it does not decode escapes or evaluate Lisp-looking
text. Numeric conversion is limited to tokens containing ASCII digits, `e` or
`E`, `.`, `+`, `-`, and spaces that parse completely as one number in the
Common Lisp reader with reader evaluation disabled and the default float
format set to double-float. For example, `1E3` becomes `1000.0`. `:colnames`
and `:rownames`
reassembly is not implemented.

The importer returns `NIL` for empty stdout or output containing only exact
empty lines. It ignores one final LF as a line terminator; if the last
remaining line is exactly empty, it returns `NIL` for the whole import. Thus
`a\n\n` returns `NIL`, while a non-final exact empty row inside a delimited
table remains data. Whitespace-only lines are data: a single whitespace-only
line collapses to the empty string, while a whitespace-only row in a multirow
result becomes an empty-string cell. For example, `a\n   \n` imports as
`(("a") (""))`. Imported tables are capped at 999 lines; overflow also returns
`NIL`.
Org 9.8.10 reports a warning for some import failures, but this helper has no
warning channel and returns `NIL` instead.

#### Value results

For `:results value`, Org appends the literal command `echo $?` after the
expanded script, including its epilogue. The runner's shell interprets that
line; the helper does not translate it per shell, so status behavior is not
guaranteed for every configured shell. When the shell recognizes `$?`, the
command prints the immediately preceding shell command's status. If the
epilogue contains a command, its final command supplies that status; otherwise
it is the last executed command before `echo $?`.

Value import selects the last nonempty stdout line and normally sends that line
through the same table importer described above. A selected line containing a
TAB can therefore become a row. If `echo $?` prints `0`, the importer reads
that output text as numeric scalar `0`; this is not an exit-code field returned
by the runner. If `echo $?` emits no line, earlier command output may supply
the value. `:results value output` and scalar-style result
parameters such as `scalar`, `verbatim`, or `code` return the selected text
directly. With `:results value table`, selected value text `0` imports as
numeric `0`. The results edit then renders it as a one-cell Org table,
represented as `((0))` in the result model.

#### Sessions and limits

The runner protocol is synchronous and returns only one stdout string; it has
no persistent session, stderr result, or separate process-status return. A Lisp
error signaled by the runner propagates from the executor. The executor receives
no separate exit code; a runner that wants a nonzero process exit to fail the
block must signal a Lisp error. The helper ignores `:async yes`. An omitted
`:session` or `:session none` allows a one-shot call. A named session signals
`org-kit:org-babel-error` unless resolved `:stdin` or `:cmdline` has a non-nil
value; then the helper ignores the name and still makes one stateless call.
`:shebang` alone does not allow a named session. For input limits, the helper
separately sums the character lengths and encoded byte lengths of the shell
name, script, stdin, and cmdline. It then checks both totals against the same
`:max-input-length` value. The default limit is 64 Mi for each total. Mi means
1,048,576 characters for character totals and encoded bytes for byte totals.
The byte count uses the document encoding, or UTF-8 when none is set.
The helper checks returned stdout against `:max-output-length` and
`:max-output-octets`, which default to 64 Mi characters and 256 Mi encoded
bytes. The byte count for `:max-output-octets` uses the same encoding. Output
checks run after the runner returns, so they cannot limit memory that the runner
allocates while producing stdout. Omitted values in `make-limits` use these
defaults. Pass `:limits (org-kit:make-limits :max-input-length N
:max-output-length N :max-output-octets N)` to `org-kit:parse-org` to configure
them.

### Execution results and controls

`org-kit:execute-babel-source-block` runs a source block through its registered
executor and returns an `org-kit:babel-execution-result`. Its status is
`:executed`, `:cached`, `:skipped`, or `:cancelled`. Executed results include
the raw value, the cache hash when enabled, and an `edit-result` for the
`#+RESULTS` update. Read the updated document through that edit result with
`org-kit:edit-result-updated-document`; the input document object is unchanged.
A skipped, cancelled, or cached result leaves the document unchanged.

Pass `:evaluation-context :export` when preparing an export. `:eval no` and
`:eval never` skip execution; `:eval no-export` skips only in export context.
For `:eval query`, pass a `:confirmation-function` that receives the block and
its effective parameters. A missing confirmation function signals
`org-kit:org-babel-error` with reason `:confirmation-required`. A non-nil return
value allows execution; `nil` returns a `:cancelled` result.

For `:cache yes`, pass a `:hash-function` that accepts the expanded body,
effective parameters, and resolved variable values, and returns a string. A
matching hash on the adjacent `#+RESULTS[hash]` line skips execution. The hash
identifies inputs for cache invalidation; it is not an integrity check. Inline
calls have no persistent `#+RESULTS` line, so they run on each request.

Pass `:result-resolver` to source-block or call execution when the block uses
executable `:var` references or `<<name()>>` noweb references. The resolver
receives the referenced block name and resolved variable alist, and returns a
Common Lisp value. The executor still receives the expanded body and resolved
variables; the library does not execute the referenced block for the resolver.

`org-kit:execute-babel-call` accepts a `#+CALL` or inline-call node. It resolves
named blocks in the current document first, then searches the supplied
`:library-documents` in order. Those documents are parsed by the caller; the
library does not load files. Call arguments and call-site headers override the
target block's matching variable and parameter values. The call node describes
the invocation; execution uses the resolved target source block and its body.

If the `subtotal` block declares `:var value=0`, then
`#+CALL: subtotal(value=4)` passes `4` as that variable's value. Parse the call
line and pass its node to `org-kit:execute-babel-call`.

For example, `*subtotal-call*` is the parsed `#+CALL: subtotal()` node from the
document fixture. Passing it with the same registry executes the named
`subtotal` block:

```lisp
(org-kit:execute-babel-call
 *babel-document* *subtotal-call* *babel-executors*)
```

`org-kit:update-babel-results` formats a supplied raw value and returns an
`edit-result`. It does not change the document object. The `output` mode accepts
only string values. Result formatting supports the requested `value` and
`output` modes, `table`, `list`, `verbatim`,
`raw`, `drawer`, `org`, `html`, and `code` formats, and `replace`, `append`,
`prepend`, and `silent` operations. Unsupported result instructions signal
`org-kit:org-babel-error` instead of being ignored.

The Common Lisp reference executor returns the final form's value for `value`
results and captured standard, error, and trace output for `output` results.
Custom executors receive the resolved `RESULTS` parameter and decide how to
produce that raw value.

Its required arguments are a document, a source-block or `#+CALL` node from
that document, and the raw result value:

```lisp
(org-kit:update-babel-results
 *babel-document* *subtotal-call* "42")
```

## Tangling

`org-kit:babel-tangle` returns an `org-kit:write-plan`. It groups blocks by
their `:tangle` path in document order and applies `:mkdirp`, `:comments`,
`:shebang`, `:padline`, and `:tangle-mode`. `:tangle yes` derives the output
name from the document source name and block language. `:target-file` supplies
the default path when `:tangle` is absent from generic or language-specific
properties, heading properties, and the source block header or switches. An
explicit `:tangle` value at any of those levels, including `:tangle no`, takes
precedence. `:language-regexp` filters blocks by language. Every plan
path is relative to the root later passed to `org-kit:apply-write-plan`; an
absolute path or parent component signals `org-kit:org-write-plan-error` while
the plan is built.

```lisp
(defparameter *tangle-document*
  (org-kit:parse-org
   (format nil
           "#+BEGIN_SRC emacs-lisp :tangle yes~%42~%#+END_SRC~%#+BEGIN_SRC emacs-lisp~%43~%#+END_SRC~%")
   :source-name "example.org"))

(defparameter *tangle-plan*
  (org-kit:babel-tangle *tangle-document* :target-file "fallback.el"))

(org-kit:write-plan-entry-path
 (aref (org-kit:write-plan-entries *tangle-plan*) 0))
;; => "example.el"

(org-kit:write-plan-entry-path
 (aref (org-kit:write-plan-entries *tangle-plan*) 1))
;; => "fallback.el"

;; Apply only after choosing a destination root.
(org-kit:apply-write-plan *tangle-plan* *destination-root*)
```

Tangling never writes files. It uses `org-kit:expand-babel-noweb` with the
`:tangle` context. `:comment-renderer` receives `(block kind target-path)`,
where `kind` is `:link` or `:org`. For `:link`, it returns the before-body and
after-body strings as two values. For `:org`, it returns the before-body string.
`:comments no` requests no comment renderer. In this library, `yes`, `link`, and
`noweb` all request the same `:link` renderer call; `org` requests `:org`; and
`both` requests `:org` followed by `:link`.
The library has no built-in comment renderer, so every requested comment mode
requires `:comment-renderer`. `:variable-renderer` receives
`(block resolved-variable-alist)` and is required for blocks with `:var`.
Pass `:result-resolver` to `babel-tangle` to resolve executable `:var` values
for that renderer and `<<name()>>` references in tangled bodies. It returns
values for the caller; it does not execute a referenced source block.
For example, a link renderer can return `"# [[file:source.org][block]]"` and
`"# block ends here"`; a variable renderer for `answer=42` can return
`"answer=42"`.
Tangle content and result formatting enforce the document's Babel and
write-plan limits before constructing output strings.

## Detangling

`org-kit:babel-detangle` takes a parsed document and tangling output text. It
returns an `org-kit:edit-result` with the updated document and source spans. It
does not read or write files or start a process.

Org's tangle comments pair a link such as `[[file:source.org::*Heading][Heading:1]]`
with an end marker such as `Heading:1 ends here`. The comment prefix depends on
the language, so detangling recognizes the Org link and end-marker text within
the rendered comment lines without requiring a particular prefix. A named
source block uses its `#+NAME` value. An unnamed block uses `Heading:n`, where
`n` is its source-block position within that heading, or `No heading:n` when it
is outside a heading. Named blocks count toward that position.

The source name is the link's display text, between `][` and `]]`; the file
target is not opened. That name resolves only against blocks in the supplied
document. A link whose display text does not match a source block is ignored,
and unrelated text is unchanged. A recognized start link must have a matching
`<source-name> ends here` line before another linked section starts; a missing
or mismatched end marker signals an error.
Detangling replaces only matched source-block bodies; text before, between, and
after those bodies remains byte-for-byte unchanged. Duplicate source names,
including collisions between a named block and a generated label, signal
`org-kit:org-babel-error`. `max-input-length` limits both strings, while
`max-search-work` bounds the cumulative marker scan. The resulting edit
enforces output character and octet limits. See [Resource limits](../reference/node-model.md#resource-limits)
for their defaults and the other Babel ceilings.

```lisp
(defparameter *detangle-document*
  (org-kit:parse-org
   (format nil "#+NAME: sample~%#+BEGIN_SRC text~%old~%#+END_SRC~%")))

(defparameter *detangle-result*
  (org-kit:babel-detangle
   *detangle-document*
   (format nil "# [[file:example.org][sample]]~%new~%# sample ends here")))

(org-kit:document-text
 (org-kit:edit-result-updated-document *detangle-result*))
```

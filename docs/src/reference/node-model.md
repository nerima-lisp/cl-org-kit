# Node Model

This page describes the parser, writers, query engine, and editor. Every
cl-org-kit API symbol named here is exported from `ORG-KIT`; Common Lisp and
keyword symbols are named as well.

## Documents

`make-document` decodes its input and returns a `document` with no tree.
`parse-org` accepts the same input forms and also parses the text, installing an
`:org-data` root on its returned document.
Pass `:settings`, `:todo-keywords`, `:startup`, `:inlinetask-min-level`, or
`:link-abbrevs` to `parse-org` to override in-buffer settings.

```lisp
(org-kit:make-document input &key source-name encoding limits)
(org-kit:parse-org input &key source-name encoding limits settings todo-keywords
                          startup inlinetask-min-level link-abbrevs)
```

For octet input, `:encoding` is forwarded to `cl-codec-kit:octets-to-string`.
Strings and character streams already contain decoded text.

```lisp
(org-kit:parse-org input :source-name "notes.org"
                   :limits (org-kit:make-limits :max-node-count 1000))
```

| Input | Decoding |
|---|---|
| string | no character decoding |
| character stream | read to the end |
| octet vector, octet stream, pathname | `encoding` if given, otherwise the Unicode encoding detected from a BOM or null-byte pattern, falling back to UTF-8 |

An explicit `encoding` must be an encoding designator accepted by `cl-codec-kit:octets-to-string`;
see the [supported encoding reference](https://nerima-lisp.github.io/cl-codec-kit/).

A leading BOM (U+FEFF) is removed from the text in every case, and
`document-bom-p` records that it was there. `document-encoding` is the encoding
used for octet input and `NIL` for character input. Only the pathname branch
opens a file; string input performs no I/O.

- undecodable octets signal `org-parse-error` whose offset is the octet index;
- an encoding the decoder does not support signals `org-kit-error`;
- input longer than `:max-input-length` (octets before decoding, characters
  for character input) signals `org-resource-limit-error`.

| Reader | Value |
|---|---|
| `document-text` | the decoded text without BOM |
| `document-limits` | the `limits` given to `make-document` or `parse-org` (or defaults); operations reuse these ceilings and do not implicitly reparse the text |
| `document-root` | the `:org-data` node installed by `parse-org`, or `NIL` after `make-document` and before parsing; `setf` installs a tree |
| `document-settings` | plist of in-buffer settings; `setf`-able |
| `document-newline` | `:lf`, `:crlf`, `:mixed`, or `:none` |
| `document-final-newline-p` | whether the text ends with a newline |
| `document-line-count` | number of lines; text ending in a newline has an empty last line |
| `offset-line-column` | 1-based line and column of an offset (the text length is allowed) |
| `line-offset` | offset at which a 1-based line starts |

`startup-options` interprets the document's `#+STARTUP` words with Org's
`org-startup-options` table. Its first value is a plist from each named Org
variable, without the `org-` prefix, to the value set by the last word naming
it: `t`, `nil`, or a keyword such as `:overview`, `:time`, `:note`, or
`:state`. For example, `#+STARTUP: overview lognotedone` yields a plist that
maps `:startup-folded` to `:overview` and `:log-done` to `:note`. The second
value lists the words Org does not recognize, in source order. A `:startup`
argument to `parse-org` replaces the document's words.

Only `#\Newline` advances to the next line. In a CRLF pair, offsets at both
`#\Return` and `#\Newline` are reported on the line being terminated; the
column at the `#\Newline` includes the preceding `#\Return`. The offset just
after `#\Newline` is column 1 of the next line. A writer that copies spans
reproduces CRLF and mixed endings exactly.

## Offsets

All offsets are 0-based character indexes into `document-text`. End offsets
are exclusive. The non-`NIL` offsets on one node are ordered as follows:

```
begin <= post-affiliated <= contents-begin <= contents-end <= end
```

`make-node` signals `org-kit-error` when the offsets it is given break this
order, and `type-error` for an unknown type. Offsets that do not apply are
`NIL`; a node built by a program has no offsets unless they are explicitly supplied.

| Slot | Meaning |
|---|---|
| `begin` | first character, including affiliated keywords such as `#+NAME:` |
| `post-affiliated` | first character after the affiliated keywords |
| `contents-begin`, `contents-end` | the span holding child nodes; `NIL` for a node without contents |
| `end` | after the node, including trailing blank lines |
| `post-blank` | blank lines after an element, or spaces after an object |

Derived positions are available on any node attached to a document:
`node-start-position`, `node-end-position` (1-based line and column),
`node-line-range` (first and last line, both inclusive, a trailing newline
counted on the line it ends) and `node-text` (the spanned source). On a node
outside a document they return `NIL`.

## Nodes

Every element and object is one `node` structure. There are no per-type
structures; type-specific data lives in the property plist.

```lisp
(org-kit:make-node type &key begin end contents-begin contents-end
                             post-affiliated (post-blank 0) properties children)
```

| Function | Purpose |
|---|---|
| `node-type` | returns a node's type keyword |
| `node-parent`, `node-children` | tree links; `(setf node-children)` re-parents, `append-child` adds one |
| `node-root`, `node-document` | root of the tree and the document it is installed in |
| `node-property` | `(node-property node key &optional default)`; second value is true when present |
| `(setf node-property)` | set one property |
| `node-properties` | fresh copy of the plist |
| `map-nodes` | call a function on a node and its descendants in document order |

`node-children` returns the internal list; do not modify it.

### Property keys

Here, `org-element` refers to Emacs Org's API for naming parsed elements and
their properties. cl-org-kit uses the corresponding property names as keywords,
for example `:raw-value`, `:level`, `:todo-keyword`, `:todo-type`,
`:priority`, `:tags`, `:archivedp`, `:commentedp` and `:title` on a headline;
`:key` and `:value` on a keyword; `:language`, `:switches`, `:parameters` and
`:value` on a src block. Node slots such as `begin`, `end`, the contents offsets,
`post-affiliated`, `post-blank`, and `parent` are separate from the property
plist and are never repeated there.
A headline `:title` can be a list of parsed nodes rather than a string.
Those nodes are children of the headline.

## Node types

`node-types` lists the 56 types below and `node-type-category` maps each to its
category. `node-type-p` tests whether a keyword belongs to this set. The type
specifier `node-type` accepts keywords from this set. The names match Emacs
Org's `org-element` API; this is a naming convention, not a claim that both
parsers produce identical trees.

| Category | Types |
|---|---|
| `:document` | `:org-data` |
| `:greater-element` | `:headline`, `:section`, `:inlinetask`, `:center-block`, `:quote-block`, `:special-block`, `:drawer`, `:property-drawer`, `:dynamic-block`, `:footnote-definition`, `:plain-list`, `:item`, `:table` |
| `:lesser-element` | `:babel-call`, `:clock`, `:comment`, `:comment-block`, `:diary-sexp`, `:example-block`, `:export-block`, `:fixed-width`, `:horizontal-rule`, `:keyword`, `:latex-environment`, `:node-property`, `:paragraph`, `:planning`, `:src-block`, `:table-row`, `:verse-block` |
| `:object` | `:bold`, `:citation`, `:citation-reference`, `:code`, `:entity`, `:export-snippet`, `:footnote-reference`, `:inline-babel-call`, `:inline-src-block`, `:italic`, `:latex-fragment`, `:line-break`, `:link`, `:macro`, `:plain-text`, `:radio-target`, `:statistics-cookie`, `:strike-through`, `:subscript`, `:superscript`, `:table-cell`, `:target`, `:timestamp`, `:underline`, `:verbatim` |

The Org syntax grouping “greater blocks” contains `:center-block`,
`:quote-block`, and `:special-block`; “lesser blocks” contains
`:comment-block`, `:example-block`, `:export-block`, `:src-block`, and
`:verse-block`. These syntax groupings are distinct from the `:greater-element`
node category. `#+CALL:` lines are `:babel-call`.

## Resource limits

`make-limits` returns resource ceilings used by parsing, editing, queries,
link resolution, agendas, clock summaries, table operations, export, Babel
result resolution, and noweb expansion. An operation that exceeds an
applicable ceiling signals `org-resource-limit-error` with the limit keyword,
configured value, and measured amount. Limits passed to `parse-org` are kept on
the document and reused by its queries, edits, and exporters.

| Keyword | Default | Bounds |
|---|---|---|
| `:max-input-length` | 64 MiB | input octets before decoding, characters otherwise |
| `:max-heading-depth` | 256 | headline level |
| `:max-element-depth` | 512 | element nesting |
| `:max-search-regex-length` | 4,096 characters | regular-expression search pattern length |
| `:max-search-work` | 64 Mi characters | candidate text submitted to regular-expression searches per document and link-resolution index |
| `:max-object-depth` | 128 | object nesting |
| `:max-node-count` | 4 Mi nodes | nodes in one document |
| `:max-output-length` | 64 Mi characters | characters emitted |
| `:max-output-octets` | 256 Mi octets | encoded output |
| `:max-agenda-range-days` | 3,660 days | inclusive days in one agenda range |
| `:max-agenda-entries` | 1,000,000 | generated entries in one agenda call |
| `:max-diary-work` | 1,000,000 | candidate-day and diary-expression pairs checked per agenda call or structured diary query |
| `:max-diary-expression-length` | 65,536 characters | one diary expression's source length |
| `:max-diary-expression-depth` | 128 | nested lists in one diary expression |
| `:max-future-repeats` | 10,000 | generated future timestamp repeats |
| `:max-subtree-clones` | 1,000 | subtree copies and repeater instances generated by one edit |
| `:max-export-expansion-depth` | 32 | recursive export expansion depth |
| `:max-export-expanded-characters` | 64 Mi characters | cumulative expanded text per export |
| `:max-export-expanded-nodes` | 4 Mi nodes | cumulative expanded nodes per export |
| `:max-babel-expansion-depth` | 32 | recursive noweb expansion depth |
| `:max-babel-expanded-characters` | 64 Mi characters | cumulative noweb expansion per call |
| `:max-babel-result-elements` | 1,000,000 | objects visited per callback result, including containers and each list cell |
| `:max-formula-tokens` | 10,000 | tokens in one table formula |
| `:max-formula-ast-depth` | 256 | syntax tree depth for one formula |
| `:max-formula-precision` | 34 | significant decimal digits |
| `:max-formula-exponent` | 6,144 | absolute decimal exponent |
| `:max-formula-work` | 1,000,000 | cumulative work units for one `table-recalculate` call |

Agenda calls over multiple documents use the strictest configured agenda
ceilings among those documents. Structural table edits insert, delete, or move
rows and columns; see the [tables guide](../guides/tables.md) for the functions
and formula syntax. `TBLFM` is an Org table-formula directive. Remapping
supports one directive whose syntax the remapper recognizes; multiple
directives or unsupported syntax signal `org-table-error`. For a supported
directive, a structural edit shifts absolute row and column coordinates in
assignment targets and references. Current-row references refer
to the formula row, relative references use offsets from the formula cell,
named-column references use header names, and hline anchors locate rows by
horizontal rules; these forms are left unchanged by remapping.
`table-recalculate` parses and evaluates supported formulas. Formula token,
syntax-depth, decimal-precision, and exponent ceilings apply to remapping and
recalculation. Decimal-precision and exponent ceilings also apply to numeric
table sorting.

Queries can use a structured list or a slash-separated headline path. The
list-form query is quoted data so Common Lisp does not evaluate it. Here `:path`
selects a root-based headline path and `:todo` matches the TODO keyword. Both
functions return headline nodes by default (`:scope :headlines`). The structured
`:diary-match` predicate selects standalone diary sexps and diary timestamps.
It requires `:scope :nodes` and an explicit `:reference-time`; the returned
nodes are the diary nodes that match that date.

~~~lisp
(org-kit:query-list
 document '(:and (:path ("Projects")) (:todo := "TODO")))
(org-kit:query-list
 document '(:diary-match) :scope :nodes
 :reference-time (cl-date-kit:make-local-date 2025 1 2))
(org-kit:query-path document "Projects/*?{todo=\"TODO\"}")
~~~

Unknown diary forms use `query-list`'s optional `:callback-registry`, which
defaults to an empty registry. Register an Emacs Lisp evaluator there to handle
those forms; without one, a matching unknown form signals
`org-elisp-evaluator-error` with reason `:unregistered`.

In a path selector, `*` matches exactly one level and `**` matches zero or more
levels. The optional `?{...}` filter supports boolean `&`, `|`, and `!` operators;
`&` binds more tightly than `|`, and parentheses group expressions.

List-form queries are copied before evaluation: `:max-node-count` counts their
cons cells and `:max-element-depth` bounds nested lists. String values in list
queries and Org match strings are bounded by `:max-input-length`. The path
filter text and its literal values use that same input limit. The number of
slash-separated path segments is bounded by both `:max-node-count` and
`:max-element-depth`; boolean filter expressions use the node-count limit and
their nesting uses the element-depth limit.
Regular-expression pattern length uses `:max-input-length`, and nested capture
groups use `:max-element-depth`. Regular expressions that match file names or
categories also check candidate text against `:max-input-length`.
Query evaluation has no separate result-count limit. The document's
`:max-node-count` bounds the candidate nodes it can visit.

## Conditions

The cl-org-kit conditions listed here inherit from `org-kit-error`.
`org-kit-error-message` returns the failure description.
The accessor functions in the table are exported from `ORG-KIT` and take the condition as their argument.
APIs may also signal Common Lisp conditions, including `type-error`.

| Condition | Accessor functions |
|---|---|
| `org-kit-error` | `org-kit-error-message` |
| `org-parse-error` | `org-parse-error-offset`, `org-parse-error-line`, `org-parse-error-column`, `org-parse-error-context`, `org-parse-error-source-name` |
| `org-query-error` | `org-query-error-query`, `org-query-error-position` |
| `org-edit-error` | `org-edit-error-target`, `org-edit-error-reason` |
| `org-resource-limit-error` | `org-resource-limit-error-limit-name`, `org-resource-limit-error-limit`, `org-resource-limit-error-actual` |
| `org-time-error` | `org-time-error-reason`, `org-time-error-context` |
| `org-agenda-error` | `org-agenda-error-reason`, `org-agenda-error-context` |
| `org-clock-error` | `org-clock-error-reason`, `org-clock-error-context` |
| `org-table-error` | `org-table-error-target`, `org-table-error-reason` |
| `org-table-unsupported-syntax` | `org-table-unsupported-syntax-formula`, `org-table-unsupported-syntax-position`, `org-table-unsupported-syntax-feature` |
| `org-export-error` | `org-export-error-backend`, `org-export-error-reason`, `org-export-error-source` |
| `org-babel-error` | `org-babel-error-reason`, `org-babel-error-context` |
| `org-export-unsupported-syntax` | `org-export-unsupported-syntax-backend`, `org-export-unsupported-syntax-source`, `org-export-unsupported-syntax-position`, `org-export-unsupported-syntax-feature` |

Input excerpts stored as condition context, query, target, formula, or source are
capped at 80 characters. Control characters and backslashes are escaped.

# Node Model

This page describes the parser, writers, query engine, and editor. Every
cl-org-kit API symbol named here is exported from `ORG-KIT`; Common Lisp and
keyword symbols are named as well.

## Documents

`make-document` normalizes its input to text and returns a `document` with no
tree. `parse-org` parses the text and installs an `:org-data` root on its
returned document.

```lisp
(org-kit:make-document input &key source-name encoding limits)
```

| Input | Decoding |
|---|---|
| string | no character decoding |
| character stream | read to the end |
| octet vector, octet stream, pathname | `encoding` if given, otherwise the Unicode encoding detected from a BOM or null-byte pattern, falling back to UTF-8 |

A leading BOM (U+FEFF) is removed from the text in every case, and
`document-bom-p` records that it was there. `document-encoding` is the encoding
used for octet input and `NIL` for character input. Only the pathname branch
opens a file; string input performs no I/O.

Failures:

- undecodable octets signal `org-parse-error` whose offset is the octet index;
- an encoding the decoder does not support signals `org-kit-error`;
- input longer than `:max-input-length` (octets before decoding, characters
  for character input) signals `org-resource-limit-error`.

| Reader | Value |
|---|---|
| `document-text` | the decoded text without BOM; every node offset indexes this string |
| `document-limits` | the `limits` given to `make-document` (or defaults); operations reuse these ceilings and do not implicitly reparse the text |
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
`NIL`; a node built by a program rather than parsed has no offsets at all.

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
| `node-type` | type keyword |
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
`:value` on a src block. Slots above (`begin`, `end`, the contents offsets,
`post-affiliated`, `post-blank`, `parent`) are never repeated as properties.
A value that org-element represents as secondary objects (a headline `:title`)
is a list of nodes whose parent is the owning node.

## Node types

`node-types` lists the 56 types below and `node-type-category` maps each to its
category. `node-type-p` and the `node-type` type test membership. The names
match Emacs Org's `org-element` API; this is a naming convention, not a claim
that both parsers produce identical trees.

| Category | Types |
|---|---|
| `:document` | `:org-data` |
| `:greater-element` | `:headline`, `:section`, `:inlinetask`, `:center-block`, `:quote-block`, `:special-block`, `:drawer`, `:property-drawer`, `:dynamic-block`, `:footnote-definition`, `:plain-list`, `:item`, `:table` |
| `:lesser-element` | `:babel-call`, `:clock`, `:comment`, `:comment-block`, `:diary-sexp`, `:example-block`, `:export-block`, `:fixed-width`, `:horizontal-rule`, `:keyword`, `:latex-environment`, `:node-property`, `:paragraph`, `:planning`, `:src-block`, `:table-row`, `:verse-block` |
| `:object` | `:bold`, `:citation`, `:citation-reference`, `:code`, `:entity`, `:export-snippet`, `:footnote-reference`, `:inline-babel-call`, `:inline-src-block`, `:italic`, `:latex-fragment`, `:line-break`, `:link`, `:macro`, `:plain-text`, `:radio-target`, `:statistics-cookie`, `:strike-through`, `:subscript`, `:superscript`, `:table-cell`, `:target`, `:timestamp`, `:underline`, `:verbatim` |

In Org Syntax, “greater blocks” is a syntax grouping for `:center-block`,
`:quote-block`, and `:special-block`; “lesser blocks” groups
`:comment-block`, `:example-block`, `:export-block`, `:src-block`, and
`:verse-block`. These syntax groupings are distinct from the `:greater-element`
node category. `#+CALL:` lines are `:babel-call`.

## Resource limits

`make-limits` returns resource ceilings used by parsing, editing, queries,
link resolution, agendas, clock summaries, table operations, and export. An
operation that exceeds an applicable ceiling signals
`org-resource-limit-error` with the limit keyword, configured value, and
measured amount. Limits passed to `parse-org` are kept on the document and
reused by its queries, edits, and exporters.

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
| `:max-future-repeats` | 10,000 | generated future timestamp repeats |
| `:max-export-expansion-depth` | 32 | recursive export expansion depth |
| `:max-export-expanded-characters` | 64 Mi characters | cumulative expanded text per export |
| `:max-export-expanded-nodes` | 4 Mi nodes | cumulative expanded nodes per export |
| `:max-formula-tokens` | 10,000 | tokens in one table formula |
| `:max-formula-ast-depth` | 256 | syntax tree depth for one formula |
| `:max-formula-precision` | 34 | significant decimal digits |
| `:max-formula-exponent` | 6,144 | absolute decimal exponent |
| `:max-formula-work` | 1,000,000 | cumulative work units for one `table-recalculate` call |

Agenda calls over multiple documents use the strictest configured agenda
ceilings among those documents. Structural table edits parse supported
`TBLFM` formulas to remap references, and `table-recalculate` parses and
evaluates them; formula token, syntax-depth, decimal-precision, and exponent
ceilings apply to both operations. Decimal-precision and exponent ceilings also
apply to numeric table sorting. The formula-work ceiling counts cumulative work
during one `table-recalculate` call.

List-form queries are copied before evaluation: `:max-node-count` counts their
cons cells and `:max-element-depth` bounds nested lists. String values in list
queries and Org match strings are bounded by `:max-input-length`. A path-filter
string and its literal values use that same limit; selector segment count and
filter-predicate nodes/depth use `:max-node-count` and `:max-element-depth`.
Regular-expression pattern length uses `:max-input-length`, and group nesting
uses `:max-element-depth`. Regex matches against `FILE` or `CATEGORY` also
check the candidate value's length against `:max-input-length`. Query
evaluation has no separate result-count limit: the document's
`:max-node-count` bounds the candidate nodes it can visit.

## Conditions

The cl-org-kit conditions listed here inherit from `org-kit-error`, whose
`org-kit-error-message` is a bounded description. APIs may also signal Common
Lisp conditions, including `type-error`.

| Condition | Readers |
|---|---|
| `org-parse-error` | `-offset`, `-line`, `-column`, `-context`, `-source-name` |
| `org-query-error` | `-query`, `-position` |
| `org-edit-error` | `-target`, `-reason` |
| `org-resource-limit-error` | `-limit-name`, `-limit`, `-actual` |
| `org-time-error` | `-reason`, `-context` |
| `org-agenda-error` | `-reason`, `-context` |
| `org-clock-error` | `-reason`, `-context` |
| `org-table-error` | `-target`, `-reason` |
| `org-table-unsupported-syntax` | `-formula`, `-position`, `-feature` |
| `org-export-error` | `-backend`, `-reason`, `-source` |
| `org-export-unsupported-syntax` | `-backend`, `-source`, `-position`, `-feature` |

Input excerpts stored in a condition (contexts, query text, edit targets) escape
control characters and backslashes and are truncated after 80 characters.

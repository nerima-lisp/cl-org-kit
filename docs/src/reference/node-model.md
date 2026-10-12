# Node Model

This page is the contract between the parsers, the writers, the query engine
and the editor. Every symbol named here is exported from `ORG-KIT`; callers
never need a symbol from another library.

## Documents

`make-document` decodes its input and returns a `document` with no tree yet.

```lisp
(org-kit:make-document input &key source-name encoding limits)
```

| Input | Decoding |
|---|---|
| string | used as is |
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
| `document-limits` | the `limits` given to `make-document` (or its defaults), for reparsing the same document under the same ceilings |
| `document-root` | the `:org-data` node, or `NIL` before parsing; `setf` installs a tree |
| `document-settings` | plist of in-buffer settings; `setf`-able |
| `document-newline` | `:lf`, `:crlf`, `:mixed`, or `:none` |
| `document-final-newline-p` | whether the text ends with a newline |
| `document-line-count` | number of lines; text ending in a newline has an empty last line |
| `offset-line-column` | 1-based line and column of an offset (the text length is allowed) |
| `line-offset` | offset at which a 1-based line starts |

Only `#\Newline` ends a line. The `#\Return` of a CRLF pair stays on the line it
terminates, so a writer that copies spans reproduces CRLF and mixed endings
exactly.

## Offsets

All offsets are 0-based character indexes into `document-text`. End offsets
are exclusive. For one node:

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

These follow Emacs org-element, so its parse of the same text can be compared
directly. Derived positions are available on any node attached to a document:
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

Property keys are the keyword form of the org-element property names for that
type, for example `:raw-value`, `:level`, `:todo-keyword`, `:todo-type`,
`:priority`, `:tags`, `:archivedp`, `:commentedp` and `:title` on a headline;
`:key` and `:value` on a keyword; `:language`, `:switches`, `:parameters` and
`:value` on a src block. Slots above (`begin`, `end`, the contents offsets,
`post-affiliated`, `post-blank`, `parent`) are never repeated as properties.
A value that org-element represents as secondary objects (a headline `:title`)
is a list of nodes whose parent is the owning node.

## Node types

`node-types` lists the 56 types below and `node-type-category` maps each to its
category. `node-type-p` and the `node-type` type test membership. The names are
org-element's.

| Category | Types |
|---|---|
| `:document` | `:org-data` |
| `:greater-element` | `:headline`, `:section`, `:inlinetask`, `:center-block`, `:quote-block`, `:special-block`, `:drawer`, `:property-drawer`, `:dynamic-block`, `:footnote-definition`, `:plain-list`, `:item`, `:table` |
| `:lesser-element` | `:babel-call`, `:clock`, `:comment`, `:comment-block`, `:diary-sexp`, `:example-block`, `:export-block`, `:fixed-width`, `:horizontal-rule`, `:keyword`, `:latex-environment`, `:node-property`, `:paragraph`, `:planning`, `:src-block`, `:table-row`, `:verse-block` |
| `:object` | `:bold`, `:citation`, `:citation-reference`, `:code`, `:entity`, `:export-snippet`, `:footnote-reference`, `:inline-babel-call`, `:inline-src-block`, `:italic`, `:latex-fragment`, `:line-break`, `:link`, `:macro`, `:plain-text`, `:radio-target`, `:statistics-cookie`, `:strike-through`, `:subscript`, `:superscript`, `:table-cell`, `:target`, `:timestamp`, `:underline`, `:verbatim` |

Org Syntax's greater blocks are the three types `:center-block`,
`:quote-block` and `:special-block`; its lesser blocks are `:comment-block`,
`:example-block`, `:export-block`, `:src-block` and `:verse-block`. `#+CALL:`
lines are `:babel-call`.

## Resource limits

`make-limits` returns the ceilings a parse or write enforces. Exceeding one
signals `org-resource-limit-error` carrying the limit keyword, its value and the
measured amount.

| Keyword | Default | Bounds |
|---|---|---|
| `:max-input-length` | 64 MiB | input octets before decoding, characters otherwise |
| `:max-heading-depth` | 256 | headline level |
| `:max-element-depth` | 512 | element nesting |
| `:max-object-depth` | 128 | object nesting |
| `:max-node-count` | 4 Mi nodes | nodes in one document |
| `:max-output-length` | 64 MiB | characters written |

## Conditions

Every condition is an `org-kit-error`, whose `org-kit-error-message` is a
bounded description.

| Condition | Readers |
|---|---|
| `org-parse-error` | `-offset`, `-line`, `-column`, `-context`, `-source-name` |
| `org-query-error` | `-query`, `-position` |
| `org-edit-error` | `-target`, `-reason` |
| `org-resource-limit-error` | `-limit-name`, `-limit`, `-actual` |

Input excerpts stored in a condition (contexts, query text, edit targets) are
passed through the internal `bounded-text`, which escapes control characters
and backslashes and truncates after 80 characters.

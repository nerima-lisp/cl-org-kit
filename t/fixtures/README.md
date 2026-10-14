# Org fixtures

This directory contains a source corpus and the corresponding trees emitted by Emacs `org-element`.
The corpus covers the elements and objects in the Org Syntax v2 reference, parser boundaries, common
settings, and input variants for CRLF, a UTF-8 BOM, a missing final newline, Japanese text, and emoji.

## Layout

- `corpus/` contains the `.org` source files.
- `expected/` contains one Common Lisp-readable S-expression for each source. The source's relative
  path is retained and `.sexp` is appended to its filename.
- `../scripts/generate-org-fixtures.el` generates the expected trees and reports type coverage.

The generated data records Emacs 31.1 and Org 9.8.10. Regenerate and check coverage from the
repository root with:

```sh
emacs --batch -Q --script scripts/generate-org-fixtures.el \
  t/fixtures/corpus t/fixtures/expected

emacs --batch -Q --script scripts/generate-org-fixtures.el \
  --check-types t/fixtures/corpus t/fixtures/expected
```

Files under `corpus/` and `expected/` are byte-sensitive test data; do not reformat them or
normalize their whitespace or line endings. Git text conversion is disabled for both trees because
CRLF, BOM, missing final newlines, and intentional trailing spaces are part of these parser inputs.

## Export oracle fixtures

`export/oracle/corpus/` contains Org documents for export cases, and `export/oracle/manifest.tsv`
lists each case and its optional include source. `export/oracle/html/`, `gfm/`, and `ascii/` contain
the corresponding full backend outputs and metadata sidecars. The generator accepts only literal
relative paths from the manifest, copies include inputs to a temporary directory, and removes that
directory after export. Kit tests use in-memory include resolvers and do not invoke Emacs.

Regenerate the export oracle from the repository root with:

```sh
emacs --batch -Q --script scripts/generate-org-fixtures.el \
  --export-oracle t/fixtures/export/oracle/corpus \
  t/fixtures/export/oracle/manifest.tsv t/fixtures/export/oracle
```

The oracle pins Emacs 31.1, Org 9.8.10, and ox-gfm 20231215.1901. The generator sets
`org-export-use-babel` to nil; under this Org version that also disables source-block header
arguments, including `:exports`. The source-block oracle therefore records Org's nil-mode output;
the kit's explicit source export modes are covered separately by policy tests.

The `export/oracle/gfm/compatibility/` manifest is a GFM-only subset of the E1 cases. Its four
source copies preserve metadata and selection pruning, explicit `EXCLUDE_TAGS`, includes, macros,
footnotes, headings, and paragraphs. Each heading has an explicit `CUSTOM_ID` where needed and
`toc:nil` avoids coupling this subset to the serializer's deterministic anchor and destination
policy. The source copies remove links, literal code, tables, and other constructs with deliberate
GFM serializer differences; the shared oracle sources and outputs remain unchanged. Their complete
outputs were generated with Emacs 31.1, Org 9.8.10, and pinned ox-gfm commit
`4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb` (version 20231215.1901; source SHA-256
`84c841f4204447eec317453ebab514741d22d4b65459a652eb87e04b29483771`).

The `export/policy/gfm/` corpus fixes those differences separately. `links.org` covers angle
destinations, escaped labels, relative and fragment links, and unsafe URL fallback. `code.org` and
`source-blocks.org` cover adaptive code delimiters and source export modes; `source-blocks.org` is
the policy copy of the E1 source-block case because ox-gfm emits fixed triple-backtick fences.
`table-structure.org` covers escaped table pipes and rows, prose hard line breaks, headings and TOC,
lists, quotes, verse, and repeated footnotes. A focused serializer assertion covers a newline within
a table cell because the Org object parser does not produce line-break objects inside table cells.
`raw.org` is exported both with raw GFM constructs disabled and with the explicit opt-in. Policy
expectations are hand-reviewed kit outputs, not Ox-generated parity data. Parity tests consume only
the GFM-specific compatibility manifest for oracle comparisons and never launch Emacs.

Export policy fixtures also record deliberate differences from Org: duplicate TITLE, AUTHOR, DATE,
LANGUAGE, SELECT_TAGS, EXCLUDE_TAGS, and MACRO declarations are rejected, and global keywords in
included or macro-expanded fragments are rejected even where Org accepts them. Multiple
`#+OPTIONS` lines are combined. E1 applies the `tasks` and `tex` options while pruning its view,
including `tasks:nil`, `tasks:todo`, `tasks:done`, and `tex:nil`; these selection options are
recognized here even though the export contract does not list them. The kit accepts only
`expand-links:nil`, because enabling link expansion would read environment variables from file
links. It accepts only `broken-links:nil` and keeps its fail-closed broken-link policy.
E1's supported source-block header interpretation lets `:exports none` suppress code and results.
E1 ignores `#+HEADER:` and `#+PROPERTY: header-args`, so those forms cannot be relied on to hide
code; it may still appear in the export.

## Table calculation oracle fixtures

`table-calc/oracle/corpus/` contains formula inputs, and `table-calc/oracle/expected/` contains
the corresponding results from `org-table-recalculate-buffer-tables`. Regenerate them from the
repository root with:

```sh
nix develop -c emacs --batch -Q --script scripts/generate-org-fixtures.el \
  --table-calc t/fixtures/table-calc/oracle/corpus \
  t/fixtures/table-calc/oracle/expected
```

The generated results record Emacs 31.1 and Org 9.8.10. The generator accepts only formulas in
its literal allowlist.

`table-calc/policy/corpus/` and `table-calc/policy/expected/` record Org results for formula forms
that T4b deliberately leaves outside its supported subset. Regenerate them with:

```sh
nix develop -c emacs --batch -Q --script scripts/generate-org-fixtures.el \
  --table-calc t/fixtures/table-calc/policy/corpus \
  t/fixtures/table-calc/policy/expected
```

The coverage check reads the generated trees and reports every `org-element` element and object
type. It also counts plain-text string leaves as `:plain-text`; plain text is not an Org object
type.

## Tree format and normalization

Each output begins with comments recording the Emacs and Org versions, then one tree rooted at
`:org-data`. Each node has a type keyword, a property list, and child nodes or strings containing
plain text. The tree keeps spans and selected semantic properties such as headline state and tags,
source-block language and body, link type, path and format, timestamps, list items, tables, and
keyword settings. The property selection in the generator is authoritative.

Offsets are zero-based decoded character positions, and `:end` is exclusive. CR and LF each count
as one position, so a CRLF pair counts as two. A leading U+FEFF is removed before parsing and is
excluded from offsets. Text properties are stripped. The generator excludes `:parent`,
`:standard-properties`, `:pre-blank`, `:post-blank`, `:CATEGORY`, and unselected parser properties
such as `:raw-link` and `:raw-value`. File-context `:path` is omitted except on link nodes.

Plain, angle, bracket, and radio links all use `:link` nodes. Their `:type`, `:path`, and `:format`
properties distinguish the forms. The generator explicitly loads `org-inlinetask` and uses its
default minimum level 15 so the level-14/level-15 boundary is stable.

## Statistics cookie fixtures

`statistics-cookies/source/` contains inputs for Org's statistics-cookie update API, and
`statistics-cookies/expected/` contains the resulting text. Regenerate the oracle outputs with:

```sh
emacs --batch -Q --script scripts/generate-org-fixtures.el \
  --statistics-cookies t/fixtures/statistics-cookies/source \
  t/fixtures/statistics-cookies/expected
```

The `todo-inline-task-open` and `todo-inline-task-done` outputs are Org 9.8.10 oracle fixtures:
inline-task TODO states do not contribute to the parent heading's TODO count. The
`todo-inline-task-before-children-policy` expected file is a kit policy fixture. Org 9.8.10
produces `[0/1]` when an inline task precedes regular child headings, while the kit intentionally
counts those regular children consistently and expects `[1/2]`. Keep this policy expectation
`[1/2]`; do not regenerate it from Org.

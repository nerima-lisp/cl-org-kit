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

Export policy fixtures also record deliberate differences from Org: duplicate TITLE, AUTHOR, DATE,
LANGUAGE, SELECT_TAGS, EXCLUDE_TAGS, and MACRO declarations are rejected, and global keywords in
included or macro-expanded fragments are rejected even where Org accepts them. Multiple
`#+OPTIONS` lines are combined. E1 applies the `tasks` and `tex` options while pruning its view,
including `tasks:nil`, `tasks:todo`, `tasks:done`, and `tex:nil`; these selection options are
recognized here even though the export contract does not list them. The kit accepts only
`expand-links:nil`, because enabling link expansion would read environment variables from file
links. It accepts only `broken-links:nil` and keeps its fail-closed broken-link policy.

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

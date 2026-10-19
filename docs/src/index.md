# cl-org-kit

`cl-org-kit` is a Common Lisp library for Org mode documents. It parses Org
Syntax v2 into source-spanned nodes, so unchanged documents can be written back
without changing their source text. `write-org` returns that source text, while
`emit-org` returns canonical Org text for a node. The library also provides
query selectors and format-preserving editors.

The 0.1.0 public API includes:

- TODO keyword editing and state transitions, TODO lists, and date-range
  agendas
- clock start and stop edits, clock summaries, and clocktable generation
- Babel header arguments, static `:var` values, and noweb expansion
- Org pipe-table formatting, row and column editing, stable sorting, and
  supported formula recalculation
- HTML, GitHub Flavored Markdown, and ASCII export
- Org crypt subtree encryption and decryption through caller-supplied callbacks

Start with [Getting Started](getting-started.md), then see the
[TODO and Agenda](guides/todo-agenda.md), [Clock](guides/clock.md),
[Structure Edits](guides/structure-edits.md), [Tables](guides/tables.md),
[Links](guides/links.md), [Org IDs](guides/org-id.md), [Babel](guides/babel.md),
[BibTeX](guides/bibtex.md), [Export](guides/export.md), and
[Org crypt](guides/crypt.md) guides. The
[API stability scope](reference/api-stability.md) describes what 0.1.0 treats
as public. Contributor workflow is in [Development](project/development.md).

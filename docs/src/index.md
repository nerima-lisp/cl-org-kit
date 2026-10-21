# cl-org-kit

`cl-org-kit` is a Common Lisp library for Org mode documents. It parses Org
Syntax v2 into source-spanned nodes, so unchanged documents can be written back
without changing their source text. `write-org` returns that source text, while
`emit-org` returns canonical Org text for a node. The library also provides
query selectors and format-preserving editors.

The 0.2.0 public API includes:

- TODO transitions and logging, property and planning edits, date-range
  agendas, and habit consistency
- Clock start and stop edits, summaries, consistency checks, and clocktable
  generation and updates
- Babel header arguments, variable resolution, noweb expansion, source-block
  execution, and `:exports` modes
- Org table formatting, row and column editing, sorting, formula recalculation,
  import and export, and transposition
- HTML, GitHub Flavored Markdown, and ASCII export with registered backends
- Org ID and link resolution, capture and refile helpers, archive and
  attachment operations, BibTeX support, Org crypt, and lint diagnostics

Start with [Getting Started](getting-started.md), then see the
[TODO and Agenda](guides/todo-agenda.md), [Clock](guides/clock.md),
[Structure Edits](guides/structure-edits.md), [Tables](guides/tables.md),
[Links](guides/links.md), [Org IDs](guides/org-id.md), [Babel](guides/babel.md),
[BibTeX](guides/bibtex.md), [Export](guides/export.md), and
[Org crypt](guides/crypt.md) guides. The
[API stability scope](reference/api-stability.md) describes what 0.2.0 treats
as public. Contributor workflow is in [Development](project/development.md).

# cl-org-kit

`cl-org-kit` is a Common Lisp library for Org mode documents. It parses Org
Syntax v2 into source-spanned nodes, so unchanged documents can be written back
without changing their source text. `write-org` returns that source text, while
`emit-org` returns canonical Org text for a node. The library also provides
query selectors and format-preserving editors.

The 0.3.0 public API includes:

- TODO transitions and logging, property and planning edits, date-range
  agendas, and habit consistency
- Agenda search and stuck-project views, category access and sorting, time
  grids and custom views, text and Org output, edit results, and bulk actions
- Clock start and stop edits, summaries, consistency checks, and clocktable
  generation and updates
- Babel header arguments, variable resolution, noweb expansion, source-block
  execution, shell execution through caller-supplied runners, `org-sbe`,
  detangling, and `:exports` modes
- Org table formatting, row and column editing, sorting, formula recalculation,
  import and export, and transposition
- Export preprocessing for built-in and evaluator-backed function macros,
  caller-resolved `#+INCLUDE` sources and targets, and supported `#+OPTIONS`
- HTML, GitHub Flavored Markdown, Markdown, Org, ASCII, LaTeX, iCalendar,
  Texinfo, and man exports, with support for registered backends; see the
  [Export](guides/export.md) guide
- Org ID and link resolution, capture and refile helpers, archive and
  attachment operations, BibTeX support, Org crypt, and all built-in Org lint
  checkers

Start with [Getting Started](getting-started.md), then see the
[TODO and Agenda](guides/todo-agenda.md), [Clock](guides/clock.md),
[Structure Edits](guides/structure-edits.md), [Tables](guides/tables.md),
[Links](guides/links.md), [Org IDs](guides/org-id.md), [Babel](guides/babel.md),
[BibTeX](guides/bibtex.md), [Export](guides/export.md),
[Org crypt](guides/crypt.md), and [Lint](guides/lint.md) guides. The
[API stability scope](reference/api-stability.md) defines the public API for
0.3.0. Contributor workflow is in [Development](project/development.md).

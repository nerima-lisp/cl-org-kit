# cl-org-kit

`cl-org-kit` is a Common Lisp library for Org mode documents. It parses Org
Syntax v2 into source-spanned nodes, so unchanged documents can be written back
without changing their source text. It also provides a canonical writer,
query selectors, and format-preserving editors.

The 0.1.0 public API includes:

- TODO keyword editing, TODO lists, and date-range agendas
- clock start and stop edits, clock summaries, and clocktable generation
- Org pipe-table formatting, row and column editing, stable sorting, and
  supported formula recalculation
- HTML and GitHub Flavored Markdown export

The 0.1.0 public API does not include ASCII export or a dedicated TODO
transition helper.

Start with [Getting Started](getting-started.md), then see the
[TODO and Agenda](guides/todo-agenda.md), [Clock](guides/clock.md),
[Tables](guides/tables.md), and [Export](guides/export.md) guides. The
[API stability scope](reference/api-stability.md) describes what 0.1.0 treats
as public. Contributor workflow is in [Development](project/development.md).

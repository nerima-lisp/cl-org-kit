# API Stability

This page defines the public API scope for version 0.3.0.

## Public surface

The public interface is the set of symbols exported from the `ORG-KIT`
package. For version 0.3.0, the public symbols are those in the
[`ORG-KIT` export list at tag `v0.3.0`](https://github.com/nerima-lisp/cl-org-kit/blob/v0.3.0/src/package.lisp).
The documented arguments, return values, conditions, and resource limits
describe those exports.

Symbols internal to `ORG-KIT`, including names written as `ORG-KIT::name`,
are implementation details. Symbols from runtime dependencies are not part of
the `cl-org-kit` API. No exported symbols are marked experimental.

See the [Export guide](../guides/export.md) for supported formats and backend
details.

### Agenda and query APIs already public in 0.1.0

The 0.1.0 agenda and query entrypoints are `agenda` for date-range agendas,
`agenda-todo-list`, `agenda-match`, and `query-match-now`.

## Version 0.2.0

`cycle-heading-todo` and `transition-heading-todo` were already public in
0.1.0. Version 0.2.0 added checks that block DONE when an unfinished descendant
or an earlier unfinished TODO in an ordered sibling sequence remains. The
parent marks that sequence with `:ORDERED: t`. An unfinished TODO ancestor can
also be blocked by an earlier unfinished TODO in its own ordered sequence.
`:NOBLOCKING: t` on the target bypasses these checks.
These changes were part of the 0.2.0 release's public API. The [TODO and Agenda
guide](../guides/todo-agenda.md) documents their return values, conditions, and
the default `:max-future-repeats` limit.

## Agenda APIs added in 0.3.0

The 0.3.0 release adds the following agenda APIs to the public interface:

- Search and stuck-project views: `agenda-search-view` and
  `agenda-stuck-projects`.
- Category access through `agenda-entry-category`; sorting through
  `:sorting-strategy` and `:user-defined-sorter` on `agenda`,
  `agenda-todo-list`, `agenda-match`, `agenda-search-view`, and
  `agenda-stuck-projects`, and under `:options` in `agenda-custom`.
- Time grids: `agenda-time-grid`, `agenda-time-grid-row`, and the row
  predicate and accessors.
- Custom views: `agenda-custom`, `agenda-custom-view`, and the view predicate
  and accessors.
- Text and Org output: `agenda-format-text` and `agenda-to-org`; edit results
  through `agenda-write-into`.
- Bulk actions: `agenda-bulk-action`.

The [TODO and Agenda guide](../guides/todo-agenda.md) documents their result
types, time-context requirements, output formats, and bulk-action behavior.

## Other APIs added in 0.3.0

- Lint checker names: `wrong-header-argument`, `wrong-header-value`, and
  `named-result`. See the [Lint guide](../guides/lint.md).
- Babel detangling: `babel-detangle`. See the [Babel guide](../guides/babel.md).
- Export entrypoints: `export-org-man`, `export-org-texinfo`, and
  `export-icalendar-documents`. See the [Export guide](../guides/export.md).
- `export-org` gains Markdown (`:md`) and Org (`:org`) backends, built-in and
  evaluator-backed macros, and caller-supplied `#+INCLUDE` text via
  `:include-resolver`, with heading, ID, and named-element targets. It also
  supports `#+OPTIONS`. See the [Export guide](../guides/export.md).

## Compatibility

The project makes no compatibility guarantee between 0.x releases. A later 0.x
release may change, add, or remove public API, including documented contracts.
`CHANGELOG.md` identifies user-visible API changes. Pin a release tag and
review its release notes when upgrading.

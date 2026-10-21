# API Stability

This page defines the public API scope for version 0.2.0.

## Public surface

The public interface is the set of symbols exported from the `ORG-KIT`
package. For version 0.2.0, the export list in `src/package.lisp` at release
commit `6b33b6f5e182` defines the public symbols. The documented arguments,
return values, conditions, and resource limits describe those exports.

Symbols internal to `ORG-KIT`, including names written as `ORG-KIT::name`,
are implementation details. Symbols from runtime dependencies are not part of
the `cl-org-kit` API. The 0.2.0 exports have no separately designated
experimental subset.

`cycle-heading-todo` and `transition-heading-todo` were already public in
0.1.0. Version 0.2.0 adds dependency checks: a transition to DONE with an
unfinished dependency signals `org-edit-error` with reason `:blocked`. These
changes are part of this release's public API. The [TODO and Agenda
guide](../guides/todo-agenda.md) documents their return values, conditions, and
repeater limit.

The 0.1.0 agenda and query APIs include `agenda` for date-range agendas,
`agenda-todo-list`, `agenda-match`, and `query-match-now`.

## Agenda APIs added after 0.2.0

The source tree containing this page adds these agenda APIs after release
commit `6b33b6f5e182`. They are not part of the released 0.2.0 public API; this
page does not assign them to a later release version. They include
`agenda-search-view` and
`agenda-stuck-projects`; sorting via `:sorting-strategy` and
`:user-defined-sorter`, plus category access through `agenda-entry-category`;
`agenda-time-grid`; custom views through `agenda-custom`; text, Org-document,
and edit output through `agenda-format-text`, `agenda-to-org`, and
`agenda-write-into`; and bulk actions through `agenda-bulk-action`. The [TODO and Agenda guide](../guides/todo-agenda.md)
covers their result types, time-context requirements, output formats, and
bulk-action behavior.
## Compatibility

While the project remains at 0.x, a later 0.x release may change, add, or remove
public API. `CHANGELOG.md` identifies user-visible API changes. Pin a release
tag and review its release notes when upgrading.

This policy covers documented contracts for exported symbols, including those
described in the guides. An exported symbol without a documented contract has
no compatibility guarantee. The policy also does not cover undocumented
behavior.

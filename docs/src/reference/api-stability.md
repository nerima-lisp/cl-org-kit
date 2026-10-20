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

TODO workflow changes are part of the 0.2.0 public API. Use
`cycle-heading-todo` to advance or reverse through a configured sequence, or
`transition-heading-todo` to select a state directly. The [TODO and Agenda
guide](../guides/todo-agenda.md) documents their return values, conditions, and
repeater limit.

The 0.1.0 agenda and query APIs include `agenda` for date-range agendas,
`agenda-todo-list`, `agenda-match`, and `query-match-now`.

## Agenda APIs added after 0.2.0

The agenda APIs in this source tree were added after the 0.2.0 release and are
not part of the released public API. They include `agenda-search-view` and
`agenda-stuck-projects`; sorting options (`:sorting-strategy` and
`:user-defined-sorter`) and category access through `agenda-entry-category`;
`agenda-time-grid`; custom view definitions through `agenda-custom`; and text,
Org-document, and edit output through `agenda-format-text`, `agenda-to-org`,
and `agenda-write-into`. The [TODO and Agenda guide](../guides/todo-agenda.md)
documents each result type, its time-context requirements, and the supported
output formats.

## Compatibility

Version 0.2.0 defines the supported interface for this release. While the
project remains at 0.x, a later 0.x release may change, add, or remove public
API. Release notes identify user-visible API changes. Pin a release tag and
review its notes when upgrading.

This policy does not guarantee compatibility for undocumented behavior or data
formats beyond the contracts stated in the reference pages.

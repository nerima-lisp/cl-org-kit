# API Stability

This page defines the public API scope for version 0.1.0.

## Public surface

The public interface is the set of symbols exported from the `ORG-KIT`
package. Its source of truth is `src/package.lisp`. The documented arguments,
return values, conditions, and resource limits describe those exports.

Symbols internal to `ORG-KIT`, including names written as `ORG-KIT::name`,
are implementation details. Symbols from runtime dependencies are not part of
the `cl-org-kit` API. The 0.1.0 exports have no separately designated
experimental subset.

TODO workflow changes are part of the 0.1.0 public API. Use
`cycle-heading-todo` to advance or reverse through a configured sequence, or
`transition-heading-todo` to select a state directly. The [TODO and Agenda
guide](../guides/todo-agenda.md) documents their return values, conditions, and
repeater limit.

## Compatibility

Version 0.1.0 defines the supported interface for that release. While the
project remains at 0.x, a later 0.x release may change, add, or remove public
API. Release notes identify user-visible API changes. Pin a release tag and
review its notes when upgrading.

This policy does not guarantee compatibility for undocumented behavior or data
formats beyond the contracts stated in the reference pages.

# Changelog

## [0.3.0]

### Added

- Add `wrong-header-argument`, `wrong-header-value`, and `named-result` checks
  to `org-kit:org-lint`, bringing the supported checker list to the 61 names
  defined by Org 9.8.10. See the [lint guide](docs/src/guides/lint.md).
- Add agenda search (`agenda-search-view`), stuck-project and custom views
  (`agenda-stuck-projects`, `agenda-custom`), time grids, sorting that preserves
  order on ties, text/Org output, and bulk actions (`agenda-bulk-action`). See
  the [agenda guide](docs/src/guides/todo-agenda.md).
- Add `org-kit:babel-detangle` to update source blocks from tangle output and
  return the change as an `edit-result`. See the
  [Babel guide](docs/src/guides/babel.md).
- Expand built-in Org macros, route function macros through a registered
  evaluator, and let `:include-resolver` supply `#+INCLUDE` text. Includes can
  select headings, `CUSTOM_ID`s, and named elements; supported `#+OPTIONS`
  tokens are applied. See the [export guide](docs/src/guides/export.md).
- Add Markdown export through `org-kit:export-org` with the `:md` backend,
  compatible with Org's `ox-md`. See the
  [export guide](docs/src/guides/export.md).
- Add Org export through `org-kit:export-org` with the `:org` backend. See the
  [export guide](docs/src/guides/export.md).
- Export one parsed document as iCalendar with `org-kit:export-org` using the
  `:icalendar` backend, or combine documents into one calendar with
  `org-kit:export-icalendar-documents`. See the
  [export guide](docs/src/guides/export.md).
- Add `org-kit:export-org-texinfo` and `org-kit:export-org-man` for Texinfo and
  man export. They return `.texi` and roff source without invoking external
  formatters. See the [export guide](docs/src/guides/export.md).

### Changed

- Update runners passed to `org-kit:make-babel-shell-reference-executor` to
  accept `(shell script stdin cmdline)` and return stdout as a string. The
  [Babel guide](docs/src/guides/babel.md) documents the `:stdin` and `:cmdline`
  header arguments.
- Babel `:results output table` preserves empty fields as `""` cells. Consumers
  that previously removed empty fields must retain them to preserve later
  columns. See the [Babel guide](docs/src/guides/babel.md).

### Fixed

- Align table-formula `org-sbe` arguments with Org 9.8.10: an empty value group
  such as `(empty)` now resolves to `nil`. Use `(empty $ "")` to pass an empty
  string explicitly. See the [Tables guide](docs/src/guides/tables.md).

### Public API

The agenda API adds category access through `agenda-entry-category` and source
editing through `agenda-write-into`, which returns an `edit-result` for
replacing a range in the input document with rendered output.

The new exports in this release are `org-kit:babel-detangle`,
`org-kit:agenda-entry-category`, `org-kit:agenda-bulk-action`,
`org-kit:agenda-search-view`, `org-kit:agenda-stuck-projects`,
`org-kit:agenda-time-grid`, `org-kit:agenda-time-grid-row`,
`org-kit:agenda-time-grid-row-p`, `org-kit:agenda-time-grid-row-date`,
`org-kit:agenda-time-grid-row-time`, `org-kit:agenda-time-grid-row-current-p`,
`org-kit:agenda-custom`, `org-kit:agenda-custom-view`,
`org-kit:agenda-custom-view-p`, `org-kit:agenda-custom-view-key`,
`org-kit:agenda-custom-view-description`, `org-kit:agenda-custom-view-type`,
`org-kit:agenda-custom-view-result`, `org-kit:agenda-format-text`,
`org-kit:agenda-to-org`, `org-kit:agenda-write-into`,
`org-kit:export-org-man`, `org-kit:export-org-texinfo`, and
`org-kit:export-icalendar-documents`.

Version 0.3.0 removes no names exported in 0.2.0.

## [0.2.0]

### Added

- Extend TODO transitions with completion and repeater logging, dependency checks,
  and a built-in `BLOCKED` query. Add habit consistency and explicit clock and
  timezone context.
- Add clock consistency checks and dynamic-block handlers. Clocktable generation
  supports range and match filters, depth and output limits, and source-preserving
  body updates.
- Improve table sorting and formula recalculation. Add table import and export,
  transposition, radio-table values, and plot write plans.
- Add registered Babel executors, source-block execution, noweb expansion, and
  result updates. Source-block `:exports` supports `code`, `results`, `both`, and
  `none`; the default is `code`.
- Add Org ID and link resolution, capture and refile helpers, archive and
  attachment operations, callback-based encryption, BibTeX import and export,
  lint diagnostics, and extensible export backends.

### Public API

New exported entry points include `org-kit:check-clock-consistency`,
`org-kit:register-dynamic-block-handler`, `org-kit:update-dynamic-block`,
`org-kit:table-import`, `org-kit:table-export`, `org-kit:table-transpose`,
`org-kit:table-send-radio`, `org-kit:table-plot-write-plan`,
`org-kit:register-babel-executor`, `org-kit:execute-babel-source-block`,
`org-kit:resolve-link`, `org-kit:generate-org-id`,
`org-kit:capture-into-target`, `org-kit:archive-to-sibling`,
`org-kit:add-attachment`, `org-kit:org-lint`, and
`org-kit:register-export-backend`.

### Behavior changes

- `org-kit:transition-heading-todo` signals `org-edit-error` with reason
  `:blocked` when an unfinished dependency prevents a transition to a DONE
  state. Add `:NOBLOCKING: t` to the target heading to opt out. The built-in
  `BLOCKED` query reports `"t"` for blocked unfinished TODO headings when no
  provider supplies a value.
- Priority edits and sorting validate `#+PRIORITIES`. The first adjustment of
  an unmarked heading uses the configured default, and crossing a range
  boundary clears the marker. Pass `:previous-direction` to continue a
  same-direction wrap after the marker clears.
- `org-kit:query-match-now` requires an explicit `:time-context`. Pass a
  reference time and zone for relative-date matching.

No exported names were removed from 0.1.0.

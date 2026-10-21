# Changelog

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

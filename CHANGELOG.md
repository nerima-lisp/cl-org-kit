# Changelog

## [0.3.0]

### Added

- Implement `wrong-header-argument`, `wrong-header-value`, and `named-result`,
  completing support for all 61 active checker names in Org 9.8.10's registry.
  Pass accepted header names and values in `:babel-header-specifications` for
  the first two; pass each block's resolved `:exports` value in
  `:babel-effective-exports` to check whether named `#+RESULTS` entries belong
  to source blocks that export results. A checker is skipped if its required
  context key is missing. See the [lint guide](docs/src/guides/lint.md).
- Add `agenda-search-view` for phrase or Boolean queries within headline
  subtrees. Boolean queries use `+term` for required terms and `-term` for
  vetoes; all required terms must match, and any veto rejects the heading.
  `agenda-stuck-projects` takes `(matcher todo-veto tags-veto subtree-regexp)`;
  `matcher` is an Org matcher string. It selects candidate headings, then
  excludes a candidate if it or a descendant has a listed unfinished TODO or
  explicit tag, or its subtree text matches the supplied regex. The TODO veto
  is a list of unfinished keywords, the tag veto a list of explicit tags, and
  the regex veto a regular expression. Custom view definitions use a unique
  key and one of `:agenda` (with `:start-date` and `:end-date`), `:todo`,
  `:match`, or `:search`, plus options for that view. Stable sorting via
  `:sorting-strategy` applies to agenda, TODO, match, search, and stuck-project
  views; criteria include category and time, and `:user-defined-sorter` accepts
  a caller comparator. `agenda-time-grid` generates rows for supplied `HHMM`
  ticks over a date range. See the
  [agenda guide](docs/src/guides/todo-agenda.md) for query syntax and options.
- Format agenda views as text or Org. `agenda-write-into` returns an edit
  result that replaces a caller-selected half-open character range in a parsed
  Org source document with `:text` output by default or `:org` via `:format`.
  `agenda-bulk-action` applies TODO state, tag, schedule, refile, and archive
  actions to a selected agenda result, custom view, or list of agenda entries.
- The existing `org-kit:make-babel-shell-reference-executor` returns an
  executor that passes `(shell script stdin cmdline)` to a caller-provided
  runner; the runner returns stdout as a string, and the helper starts no
  process. In table formulas, `org-sbe` calls named source blocks through the
  executor registered for their language. `babel-detangle` matches the display
  name in a tangle-comment link against a named source block or an unnamed
  block's generated label. `Heading:n` uses the containing heading's text and
  the source-block position within that heading; named blocks count toward the
  position. Outside a heading, the label is `No heading:n`.
  It returns an `edit-result` that replaces the matched body and preserves
  surrounding text. See the [Babel guide](docs/src/guides/babel.md).
- Combine multiple parsed Org documents into one iCalendar calendar
  (`VCALENDAR`) with `org-kit:export-icalendar-documents`. Pass `:context` with
  `:time-context` for `DTSTAMP`. A heading's own `ID` property becomes its event
  UID; otherwise, pass `:random-source`, a zero-argument function that returns
  one octet per call. Keep IDs unique across the combined documents. See the
  [export guide](docs/src/guides/export.md) for `:context`, `:time-context`,
  and `:random-source` options. Texinfo export returns
  `.texi` source and man export returns roff source without invoking external
  formatters.

### Public API

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

No exported names were removed from 0.2.0.

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

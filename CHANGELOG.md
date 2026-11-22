# Changelog

## [Unreleased]

### Changed

- `update-dynamic-block` writes the clocktable layout that Emacs writes with
  Org 9.8.10. A report over several files (`:scope agenda`, `agenda-with-archives`, a
  resolver selector, or a list of selectors) is one table with a `File` column,
  an `ALL *Total time*` row, and a `*File time*` row for each file, instead of
  a `File: <path>` line and a separate table per file. The same layout applies
  to a single file in agenda scope. `file-with-archives` is one extended file
  and has no file rows. The file label is the file name without its directory.
  Callers that parse `File:` lines must read the `File` column instead.
- A clocktable written with a `:time-context` starts with
  `#+CAPTION: Clock summary at [<time>]`, using the context's reference time
  in its zone, followed by `, for <period>.` when the block has `:block`. A block
  updated without a time context has no caption, and `:step` tables have none.
- Clocktable time columns follow `:maxlevel` up to the deepest level that has
  time, so `:maxlevel 3` writes three time columns when level 3 has clocked
  time. Earlier versions stopped at two.
- A report with no clocked time writes only the header and the total row, so
  `:fileskip0` over a period without clock data no longer leaves the block
  empty. In a report with clocked time, `:fileskip0` leaves out the files that
  have none.
- Numeric time and percentage columns written by `update-dynamic-block` are
  right-aligned as Emacs aligns them, and a link counts as its visible text
  when columns are padded. `write-clocktable` keeps its alignment, its limit of
  two time columns, and its lack of a caption.
- `:formula %` percentages, in `update-dynamic-block` and in `write-clocktable`,
  are computed in double-float arithmetic and rounded as Emacs's `%.1f` does.
  Before, single-float arithmetic and `~,1F` rounded a share that falls exactly
  on a rounding boundary upward: 1 minute of 16 printed `6.3` and now prints
  `6.2`. Shares away from a rounding boundary print as before.

### Fixed

- A clocktable block with no `:block`, `:tstart`, or `:tend` signaled
  `org-time-error` when its `:time-context` used a time zone other than UTC. The
  all-time range now uses local bounds that any zone resolves.

## [0.3.3]

### Changed

- `set-planning`, `remove-planning`, and the CLOSED timestamp that
  `transition-heading-todo` writes now follow Org's `org-add-planning-info`.
  A keyword that is set goes before the keywords already on the planning line,
  so completing a heading that has `SCHEDULED:` with `:preserve-scheduled-p t`
  produces `CLOSED: [...] SCHEDULED: <...>`, where 0.3.2 wrote `SCHEDULED: <...>
  CLOSED: [...]`.
  Setting a keyword that is already on the line moves it to the front.
  `remove-planning` on the last keyword deletes only the planning line and keeps
  the blank lines after it; 0.3.2 deleted those blank lines too. See the
  [structure edits guide](docs/src/guides/structure-edits.md#planning-lines).
- `set-planning` keeps an existing `CLOSED:` timestamp when it sets `DEADLINE:`
  or `SCHEDULED:`. Emacs `org-deadline` and `org-schedule` delete `CLOSED:` in
  that case: on `CLOSED: [2026-11-20 Fri 10:00]`, `(org-deadline nil
  "2026-11-30")` leaves `DEADLINE: <2026-11-30 Mon>`, while `set-planning` with
  `:deadline` leaves `DEADLINE: <2026-11-30 Mon> CLOSED: [2026-11-20 Fri
  10:00]`. Call `remove-planning` with `:closed` as well to match Emacs.
- A line directly under a heading that starts with `CLOSED:`, `DEADLINE:`, or
  `SCHEDULED:` is a planning line, as in Org, even when it is malformed. 0.3.2
  read `SCHEDULED: junk` or `SCHEDULED: <2026-10-11 Sun> trailing` as a
  paragraph. Now the line is a planning element: a field with no valid timestamp
  has no value, text after a valid field stays on the line, and a range
  `<a>--<b>` is one field. The kit still needs a space between the keyword and
  its timestamp, and a later keyword without a timestamp does not clear an
  earlier value. Effects, compared with 0.3.2:
  - `set-planning` writes into the existing planning line (`SCHEDULED: junk`
    becomes `SCHEDULED: <2026-11-30 Mon> SCHEDULED: junk`, as in Org), and
    rejects timestamp text that has anything after the timestamp, including
    another planning field.
  - `remove-planning` removes only the field and leaves other text on the
    line.
  - `transition-heading-todo` writes `CLOSED:` into such a line but leaves its
    other fields in place when it would remove them.
  - `emit-org` signals `org-kit-error` for a parsed planning line that has text
    outside its fields, where it used to emit the paragraph.
  - Queries and agenda functions see the timestamps on such a line.

### Fixed

- Completing a heading with `transition-heading-todo` when its only planning
  entry is a non-repeating `SCHEDULED:`, which the kit removes by default, now
  leaves exactly `CLOSED: [...]` on the planning line, with no space after it.
  0.3.2 left ` CLOSED: [...]` with a space before it, and for an indented
  planning line it dropped the indentation; the line now keeps its indentation.
- Setting a planning field with `set-planning` no longer signals
  `org-edit-error` with reason `:unexpected-diff` on a heading whose planning
  line is malformed, such as `SCHEDULED: junk`, when a `:PROPERTIES:` drawer
  follows it.
- A `:PROPERTIES:` drawer after a planning line that starts with `CLOSED:` is
  parsed as the heading's property drawer. It was parsed as an ordinary drawer,
  so `set-property` and `archive-subtree-to` signalled `org-edit-error` with
  reason `:unexpected-diff` on any heading completed in Emacs, and
  `remove-property` and the property queries did not see its properties.
- A second line that looks like planning, such as `DEADLINE:` after a
  `SCHEDULED:` line, is an ordinary paragraph, and the drawer after it is an
  ordinary drawer, as in Org. It was parsed as a property drawer.
- A planning line that is indented now also introduces a property drawer.
- `set-planning` and `transition-heading-todo` no longer signal
  `org-edit-error` with reason `:unexpected-diff` on a heading that has no body
  and is followed by a blank line.

Version 0.3.3 removes no names exported in 0.3.2.

## [0.3.2]

### Added

- Clocktable blocks accept `:link t`. Headings in another file use a relative
  file link, and headings in the same file use a link of the form
  `[[*Heading][Heading]]`. Pass `:source-name` to `parse-org` for the block
  document and the resolved document, using absolute paths for both or relative
  paths for both. The clocktable signals `org-clocktable-parameter-error` when
  either name is missing, when one name is absolute and the other relative, or
  when `..` components prevent computing a relative path. See the
  [clock guide](docs/src/guides/clock.md).
- Export the `org-clock-warning` condition, with `org-clock-warning-line` and
  `org-clock-warning-source`. `sum-clocks` and `clocktable-data` signal it for a
  CLOCK row they cannot parse and leave that row out of the totals.

### Changed

- `sum-clocks`, `clocktable-data`, and clocktable blocks total clock time the way
  Emacs `org-clock-sum` does. A closed CLOCK range is computed from the
  timestamps at both ends and clipped to the requested period. A row with a
  duration of zero or less adds nothing, and overflowing hour, minute, and day
  fields carry into the next unit. A duration-only row (`CLOCK: => 1:30`) is
  counted even when the requested period has zero length. A row that pairs a
  diary, active, or single timestamp with a `=>` duration, such as
  `CLOCK: <%(diary-float t 4 2)> => 0:10`, no longer adds that duration. Totals
  can differ from 0.3.1 for rows like these.
- A CLOCK row that `sum-clocks` or `clocktable-data` cannot parse no longer
  signals `org-clock-error`. It signals `org-clock-warning` and is left out of
  the totals; code that handled `org-clock-error` for these functions must
  handle the warning instead. The clock consistency check and clock editing
  still parse CLOCK rows strictly and signal `org-clock-error`.
- `capture-into-target` with a `:file+olp+datetree` target writes day headings
  with the full weekday name, as Emacs does (`2024-02-19 Monday`, not
  `2024-02-19 Mon`). Callers that look for the abbreviated form in the updated
  text must match the full name.

### Fixed

- `capture-into-target` with a `:file+olp+datetree` target matches existing year,
  month, and day headings by date, so existing headings that spell the weekday
  differently (for example `2024-02-19 Mon`) are reused.

Version 0.3.2 removes no names exported in 0.3.1.

## [0.3.1]

### Fixed

- `org-kit:transition-heading-todo` can retain a non-repeating `SCHEDULED`
  timestamp when completing a heading with `:preserve-scheduled-p t`. The keyword
  defaults to `nil`; when a repeater fires, the existing schedule handling
  still applies.
- `org-kit:table-align` accepts `:numeric-alignment-p t` to right-align columns
  whose non-empty data cells are all numeric. The keyword defaults to `nil`, so
  callers keep the existing left-alignment behavior unless they opt in.

Version 0.3.1 removes no names exported in 0.3.0.

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

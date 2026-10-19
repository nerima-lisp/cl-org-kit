# Clock

`clock-in`, `clock-out`, `sum-clocks`, and `clocktable-data` use explicit time
values and an explicit `org-time-context`, created with `make-time-context` from
a reference time, time zone, and daylight-saving policy. The context anchors
checks of running clocks; edit calls also take their own `:at` value. These
calls do not consult the system clock. A daylight-saving transition can create
a gap, where local date-times do not exist, or an overlap, where they occur
twice. The `:dst-policy` key selects `:compatible` (the default), `:earlier`,
`:later`, or `:strict`:

Fixed-zone offsets passed to `make-fixed-time-zone` are in minutes, so an offset
of `0` means UTC.

| Policy | Overlap | Gap |
| --- | --- | --- |
| `:compatible` | Choose the earlier occurrence | Shift forward by the gap |
| `:earlier` | Choose the earlier occurrence | Shift back by the gap |
| `:later` | Choose the later occurrence | Shift forward by the gap |
| `:strict` | Reject the local date-time | Reject the local date-time |

Examples require the project and its runtime dependencies on ASDF's
`CL_SOURCE_REGISTRY`. For repository development, see
[Development](../project/development.md); for ASDF setup prerequisites, see
[Getting Started](../getting-started.md#asdf).

## Start and stop a clock

`clock-in` and `clock-out` make format-preserving edits that leave unrelated
source text in its original form. Each returns two values: an `edit-result` and
a string. `clock-in` returns the start timestamp;
`clock-out` returns the interval string in `START--END => H:MM` form. Use the
updated document from the result before making another edit. Both calls require
explicit `:at` and `:time-context` arguments; `:at` must be an `org-time-value`.
The context's zone and daylight-saving policy resolve an offset-free `:at`; an
explicit offset on `:at` determines its instant. An edit's `:at` may be before
or after the context's reference time. `clock-out` still requires `:at` to be
at or after the recorded clock start. The reference time supplies the endpoint
for running-clock checks.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(labels ((first-headline (document)
           (let ((found nil))
             (org-kit:map-nodes
              (lambda (node)
                (when (and (null found)
                           (eq (org-kit:node-type node) :headline))
                  (setf found node)))
              (org-kit:document-root document))
             found)))
  (let* ((document (org-kit:parse-org (format nil "* Task~%")))
         (time-context
           (org-kit:make-time-context
            :reference-time
            (org-kit:make-org-time-value "2025-01-01T10:30:00")
            :zone (org-kit:make-fixed-time-zone 0))))
    (multiple-value-bind (started start-timestamp)
        (org-kit:clock-in
         document (first-headline document)
         :at (org-kit:make-org-time-value "2025-01-01T09:00:00")
         :time-context time-context)
      (let* ((started-document
               (org-kit:edit-result-updated-document started))
             (started-heading (first-headline started-document)))
        (multiple-value-bind (finished interval)
            (org-kit:clock-out
             started-document started-heading
             :at (org-kit:make-org-time-value "2025-01-01T10:30:00")
             :time-context time-context)
          (list start-timestamp interval
                (org-kit:write-org
                 (org-kit:edit-result-updated-document finished))))))))
~~~

Set `CLOCK_INTO_DRAWER` and `LOG_INTO_DRAWER` with document properties such as
`#+PROPERTY: CLOCK_INTO_DRAWER LOGBOOK`, or in a heading's `:PROPERTIES:` drawer.
For these properties, a heading value overrides ancestor values, with the
nearest heading taking precedence; the document property is the fallback.
`clock-in` uses `:drawer` when supplied; otherwise it uses the inherited
`CLOCK_INTO_DRAWER` value, defaulting to LOGBOOK when unset. Explicit `nil` for
`:drawer` is invalid. Pass a drawer name as a string or a nonnegative integer
threshold. The Lisp symbol `t` is not accepted, while the string `"t"` has the
alias behavior described below. The selected value has one of these effects:

- A drawer name passed as a string selects that drawer.
- In `CLOCK_INTO_DRAWER`, `t` means use the inherited `LOG_INTO_DRAWER` drawer
  name, or LOGBOOK if it is unset. The string `"t"` passed as `:drawer` is a
  case-insensitive alias for that behavior, so it cannot select a drawer
  literally named `t`.
- A nonnegative integer counts every existing CLOCK entry on the target heading
  across all drawers, plus the new entry. At or above the threshold, the new
  entry goes to LOGBOOK; otherwise it is written without a drawer. For example,
  a threshold of 2 sends the second CLOCK entry to LOGBOOK.

`clock-in` refuses to start a second running clock on the target heading;
`clock-out` requires exactly one running clock on that heading. `clock-out`
accepts `:drawer` for API symmetry but ignores it; the existing clock stays
where it is. Both return edits without changing the input document.

## Check clock consistency

`check-clock-consistency` returns findings in source order. It reports clock
ranges whose end precedes their start, overlapping ranges, clocks that have no
clock-out, and intervals longer than `:max-duration-minutes` when that optional
threshold is supplied. Touching ranges do not overlap. For a running clock,
the required `org-time-context`'s reference instant is the endpoint used to
measure its duration and check overlap with closed clocks. This check ignores
duration-only CLOCK rows, such as `CLOCK: => 0:30`. Malformed CLOCK rows signal
`org-clock-error` with reason `:malformed-clock`.

`check-clock-consistency` returns `clock-consistency-issue` values.
`clock-consistency-issue-reason` and `clock-consistency-issue-line` return the
reason and first source line; for an overlap,
`clock-consistency-issue-other-line` returns the second line.
`clock-consistency-issue-duration-minutes` returns elapsed minutes for
`:too-long` and `:open-clock` results and `nil` for other reasons.
`make-clock-consistency-issue` constructs a finding of the same type for callers
that create their own clock findings; the existing accessors read its fields.
Its duration accessor returns the supplied `:duration-minutes`, or `nil` when
omitted.
Pass `:reason` as `:end-before-start`, `:overlap`, `:open-clock`, or `:too-long`,
and `:line` as a positive, one-based source line. `:other-line` accepts `nil` or
a positive line. `:duration-minutes` accepts `nil` or a non-negative rational.
Both `:other-line` and `:duration-minutes` are optional for every reason.
Invalid values signal `type-error`.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil
                  "* Task~%CLOCK: [2025-01-01 Wed 09:00]--[2025-01-01 Wed 10:45] => 1:45~%")))
       (time-context
         (org-kit:make-time-context
          :reference-time (org-kit:make-org-time-value "2025-01-01T12:00:00")
          :zone (org-kit:make-fixed-time-zone 0))))
  (org-kit:check-clock-consistency
   document :time-context time-context :max-duration-minutes 90))
~~~

## Insert timer items

Org timer items are description-list items with an elapsed `H:MM:SS` value, as
in `- 0:01:23 :: Review`. The start time is supplied by the caller, and the
reference instant in `org-time-context` supplies the current time.
`timer-item-value` returns the elapsed string. `insert-timer-item` inserts a
single-line description at the start of the heading body, after its planning
entries and property drawers, and after a leading LOGBOOK drawer if present. It
goes before any other drawer or body content. The function returns
the `edit-result` and inserted value. The input document is unchanged; use the
updated document from the result. A start time after the reference instant
signals `org-clock-error` with reason `:invalid-range`.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(labels ((first-headline (document)
           (let ((found nil))
             (org-kit:map-nodes
              (lambda (node)
                (when (and (null found)
                           (eq (org-kit:node-type node) :headline))
                  (setf found node)))
              (org-kit:document-root document))
             found)))
  (let* ((document (org-kit:parse-org (format nil "* Task~%")))
         (heading (first-headline document))
         (start (org-kit:make-org-time-value "2025-01-01T10:00:00"))
         (time-context
           (org-kit:make-time-context
            :reference-time (org-kit:make-org-time-value "2025-01-01T10:01:23")
            :zone (org-kit:make-fixed-time-zone 0))))
    (list
     (org-kit:timer-item-value start time-context)
     (multiple-value-bind (edit value)
         (org-kit:insert-timer-item document heading start "Review"
                                    :time-context time-context)
       (list value
             (org-kit:write-org
              (org-kit:edit-result-updated-document edit)))))))
~~~

This follows Org's description-list timer item syntax from the
[Org manual's timer section](https://orgmode.org/manual/Timers.html).

## Effort estimates

`effort-duration-minutes` parses an Org duration and returns exact minutes.
It accepts clock forms such as `1:30` and `1:23:45`, unit forms such as `2d`
and `1h 30min`, and bare numbers interpreted as minutes. Units are `min`, `h`,
`d`, `w`, `m`, and `y`; weeks are seven days, months are 30 days, and years
are 365.25 days. Duration values do not accept a leading sign. Fractional
results remain exact rationals.
`subtree-effort-minutes` adds the direct `EFFORT` property on each heading in a
selected subtree; an inherited property is not counted repeatedly.

~~~lisp
(org-kit:effort-duration-minutes "1d3h5min")
;; => 1625
~~~

## Sum clock time

`sum-clocks` totals a heading and its descendants over the half-open interval
`[FROM, TO)`. It requires `:time-context`. The default excludes running clocks.
It includes a duration-only CLOCK row's full duration by default because the row
has no dates to clip; it does so only when `[FROM, TO)` has positive length.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(labels ((first-headline (document)
           (let ((found nil))
             (org-kit:map-nodes
              (lambda (node)
                (when (and (null found)
                           (eq (org-kit:node-type node) :headline))
                  (setf found node)))
              (org-kit:document-root document))
             found)))
  (let* ((document
           (org-kit:parse-org
            (format nil
                    "* Task~%CLOCK: [2025-01-01 Wed 09:00]--[2025-01-01 Wed 09:30] => 0:30~%")))
         (time-context
           (org-kit:make-time-context
            :reference-time (org-kit:make-org-time-value "2025-01-01T10:00:00")
            :zone (org-kit:make-fixed-time-zone 0)))
         (summary
           (org-kit:sum-clocks
            document (first-headline document)
            (org-kit:make-org-time-value "2025-01-01T09:00:00")
            (org-kit:make-org-time-value "2025-01-01T10:00:00")
            :time-context time-context)))
    (list (org-kit:clock-summary-total-minutes summary)
          (org-kit:clock-summary-scoped-minutes summary)
          (org-kit:clock-summary-unscoped-minutes summary))))
~~~

The result is a `clock-summary`. `clock-summary-scoped-minutes` counts dated
CLOCK intervals after clipping to `[FROM, TO)`. `clock-summary-unscoped-minutes`
returns the total duration-only minutes. `clock-summary-total-minutes` returns
the sum, and `clock-summary-clipped-interval-count` counts dated intervals with
positive length after clipping. `:include-running-p t` includes running clocks
through the context's reference instant. `:include-unscoped-p nil` excludes
duration-only rows. `sum-clocks` accepts optional `:as-of` as an assertion for
callers that already carry a separate time value. It verifies that the value
resolves to the same instant as the context's reference time; it does not set a
cutoff.

## Generate a clocktable

`clocktable-data` returns a `clocktable-result` for a heading subtree. It
requires a `:range`, a `:time-context`, and accepts optional Org `:match`
syntax, such as `TODO="TODO"` to select TODO headings; see the
[agenda match example](todo-agenda.md#build-a-todo-list-and-agenda). It also
accepts `:max-depth` (default 30), which filters by absolute logical
heading level, not distance from the starting heading. Level 1 is a top-level
heading; for example, a clocktable rooted at a level-2 heading with
`:max-depth 2` includes that heading but excludes its level-3 descendants.
Running clock rows are excluded. The result contains total minutes and rows in
document order. Its row accessors are `clocktable-row-level`,
`clocktable-row-title`, `clocktable-row-tags`, `clocktable-row-range`, and
`clocktable-row-minutes`. The row title excludes the TODO keyword, priority
marker, leading `COMMENT` marker, and trailing tags. Inline Org markup remains
unformatted. Tags are returned in specificity order: unique file tags from
`#+FILETAGS`, ancestor tags from root to parent, then the heading's own tags.
Duplicate tags appear once at their most specific position. Range is the
supplied `org-time-range`; minutes are clipped over its start-inclusive,
end-exclusive interval. Each row's minutes include matching clocks on its
heading and descendants in the requested subtree. The same clock can contribute
to ancestor and descendant rows. The result total counts each matching clock
once across the subtree instead of summing the row minutes.

`write-clocktable` replaces the body of a parsed `clocktable` dynamic block
with that result and returns an `edit-result`. Its `:tags-p`, `:emphasize-p`,
and `:formula` options add the corresponding clocktable formatting. Use it when
you already have a `clocktable-result`; `update-dynamic-block` parses the block
parameters and dispatches to the registered handler.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(labels ((first-node-of-type (document wanted-type)
           (let ((found nil))
             (org-kit:map-nodes
              (lambda (node)
                (when (and (null found)
                           (eq (org-kit:node-type node) wanted-type))
                  (setf found node)))
              (org-kit:document-root document))
             found)))
  (let* ((document
           (org-kit:parse-org
            (format nil
                    "* Task~%:LOGBOOK:~%CLOCK: [2025-01-01 Wed 09:00]--[2025-01-01 Wed 09:30] => 0:30~%:END:~%#+BEGIN: clocktable~%#+END:~%")))
         (heading (first-node-of-type document :headline))
         (dynamic-block (first-node-of-type document :dynamic-block))
         (time-context
           (org-kit:make-time-context
            :reference-time (org-kit:make-org-time-value "2025-01-01T10:00:00")
            :zone (org-kit:make-fixed-time-zone 0)))
         (range
           (org-kit:make-time-range
            (org-kit:make-org-time-value "2025-01-01T09:00:00")
            (org-kit:make-org-time-value "2025-01-01T10:00:00")
            :include-start-p t :include-end-p nil))
         (data
           (org-kit:clocktable-data document heading
                                    :range range :time-context time-context))
         (edit (org-kit:write-clocktable document dynamic-block data)))
    (list (org-kit:clocktable-result-total-minutes data)
          (org-kit:write-org (org-kit:edit-result-updated-document edit)))))
~~~

`update-dynamic-block` dispatches by dynamic block name. The built-in handlers
are `clocktable` and `columnview`; see [Column view](columnview.md) for the
latter. Register a custom handler with the parameter names it accepts:

~~~lisp
(org-kit:register-dynamic-block-handler
 "summary"
 (lambda (document block parameters &key document-resolver time-context)
   (declare (ignore document block document-resolver time-context))
   (format nil "Summary: ~A" (getf parameters :label)))
 :parameters '(:label))
~~~

The handler receives the parsed parameter plist and optional document resolver
and time context. It can return replacement body text, an `edit-result`, or a
list of table rows. Each row is a list of string cells; the first row, when
present, provides the header. `update-dynamic-block` returns an `edit-result`;
unregistered names and undeclared parameters signal `org-dynamic-block-error`.

For the built-in clocktable handler, `:scope` accepts `file`, `nil`, `subtree`,
`tree`, and `treeN`. `file` and `nil` select the current document. `subtree`
selects the nearest enclosing heading and its descendants. `tree` selects the
top-level heading containing the block; `treeN`, for positive N, selects the
nearest enclosing heading at absolute level N and its subtree. If no such
heading encloses the block, `update-dynamic-block` signals
`org-clocktable-parameter-error`. Other string selectors are
passed unchanged to the `document-resolver`; a list of strings is resolved one
selector at a time. `agenda`, `agenda-with-archives`, and `file-with-archives`
map to `:agenda`, `:agenda-with-archives`, and `:file-with-archives`,
respectively, when passed to the resolver. The library does not interpret other
selector syntax or open files. The resolver returns a parsed document or a
nonempty list of parsed documents.

Clocktable parameter names are case-sensitive. `:maxlevel` limits displayed
depth while deeper clock time remains in its visible ancestor row. This differs
from `clocktable-data`'s `:max-depth`, which filters returned rows by absolute
heading level. The default `:maxlevel` is 2. `:block` accepts a period such as
`2026-10-12`. Alternatively, supply both `:tstart` and `:tend` as Org date
literals, for example `:tstart "[2026-10-12 09:30]" :tend 2026-10-14`.
`:step` requires one of these range forms. Without a range, the clocktable
includes clocks from all dates and can be updated without a time context. Clock
timestamps without an explicit offset use UTC in that case; pass
`:time-context` to choose another zone. `:step` splits a selected range into
`day`, `week`, `semimonth`, `month`, `quarter`, or `year` sections. `:match`
filters headings. `:tags`, `:emphasize`, and `:fileskip0` control table
formatting. `:formula` accepts a table formula or `%` for time percentages.
Unsupported or invalid clocktable parameters signal
`org-clocktable-parameter-error` with the parameter name and reason.
For `org-dynamic-block-error` conditions, `org-dynamic-block-error-value`
returns the parsed parameter value recorded on the condition.
`org-clocktable-parameter-error-value` returns the parameter value as a string
with control characters escaped and input truncated after 80 characters.

Invalid clock rows, ranges, or options can signal `org-clock-error` or
`org-time-error`. Clock edits can also signal `org-edit-error`. Clock operations
enforce the document's `:max-node-count`; consistency checks also enforce
`:max-clock-consistency-issues`; clocktable writes enforce `:max-output-length`
and `:max-output-octets`. Their defaults are 4,194,304 nodes, 100,000 findings,
67,108,864 output characters, and 268,435,456 output octets. Configure these
limits by passing an `org-kit:make-limits` value as `:limits` to `parse-org`.
Exceeding a configured limit signals `org-resource-limit-error`.

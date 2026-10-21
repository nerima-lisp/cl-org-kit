# TODO and Agenda

Examples require the project and its runtime dependencies on ASDF's
`CL_SOURCE_REGISTRY`. For repository development, see
[Development](../project/development.md); for ASDF setup prerequisites, see
[Getting Started](../getting-started.md#asdf).

## Set a TODO keyword

`set-heading-todo` sets the keyword supplied by the caller. The keyword must be
configured through a `#+TODO` line, the `:todo-keywords` argument passed to
`parse-org`, or `:todo-keywords` in the `:settings` plist passed to `parse-org`.
Pass `nil` to remove a TODO keyword. The function returns the updated
document; the original document remains unchanged. Each edit reparses the
document, so use `map-nodes` to find the target headline in the returned document
before making another edit. A headline node from the old parse cannot be reused.

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
            (format nil "#+TODO: TODO NEXT | DONE~%~%* TODO Prepare notes~%")))
         (heading (first-node-of-type document :headline))
         (updated (org-kit:set-heading-todo document heading "NEXT")))
    (org-kit:write-org updated)))
~~~

An unknown keyword, invalid target, or conflicting edit signals
`org-edit-error`.

## Set and adjust priorities

`#+PRIORITIES` lists the highest, lowest, and default values, in that order.
For example, `#+PRIORITIES: A C B` uses the alphabetic range A through C and
defaults unmarked headings to B. Numeric ranges use values from 0 through 64,
such as `#+PRIORITIES: 1 10 5`. The two endpoints and default must all use
letters, or all use decimal numbers; the default must lie between the endpoints.
Only the first three values are used; directives with fewer than three values
are ignored. Without the directive, Org's A, C, and B defaults apply.

`set-heading-priority` accepts an uppercase letter or a numeric priority and
rejects values outside the configured range. Pass `nil` to clear the priority.
`adjust-heading-priority` moves one step toward the highest priority with `:up`
or toward the lowest with `:down`. Pass the direction as its third positional
argument, for example `(org-kit:adjust-heading-priority document heading :up)`;
`:previous-direction` is a keyword option. The first adjustment of an unmarked
heading assigns the configured default. Moving past a range endpoint removes
the priority marker. Both priority functions return the updated document, and
each edit reparses it. To wrap from A to C with `:up`:

1. Call `(org-kit:adjust-heading-priority document heading :up)`. It removes
   `[#A]` and returns `updated-document` with the heading unmarked.
2. Run the same `map-nodes` selection on `updated-document` to get
   `updated-heading`. The original headline node belongs to the old parse.
3. Call
   `(org-kit:adjust-heading-priority updated-document updated-heading :up :previous-direction :up)`.
   `:previous-direction` tells this call that the previous `:up` already
   cleared the marker, so this call sets `[#C]`.

To wrap from C to A, use `:down` for both calls and pass
`:previous-direction :down` on the second. Without a matching
`:previous-direction`, an unmarked heading starts at the configured default.
When a directive has at least three values, its first three values must define
a valid range and default; otherwise a priority edit or priority sort signals
`org-edit-error` with reason `:invalid-value`. Invalid directions and priority
values signal the same condition. `sort-subtree-children` accepts `:priority`
as its key, for example
`(org-kit:sort-subtree-children document heading :priority)`, and validates
the same directive. During `query-list` matching, numeric `PRIORITY` values are
converted to decimal text: priority 5 matches
`(:property "PRIORITY" := "5")`.
`sort-subtree-children` returns an `edit-result`; read the reordered document
with `(org-kit:edit-result-updated-document result)`. The input document remains
unchanged.

## Transition a TODO state

`transition-heading-todo` sets a heading to a configured state, while
`cycle-heading-todo` advances or reverses through a configured TODO sequence.
Pass `:direction :next` to advance or `:direction :previous` to reverse with
`cycle-heading-todo`. The text in brackets after `#+TODO` is the sequence ID;
for example, `#+TODO[FlowId]: TODO | DONE` has ID `"FlowId"`. Without
`:sequence-id`, cycling selects the configured sequence containing the heading's
current TODO keyword. `parse-org` rejects duplicate keywords among active TODO
sequences. Supplying `:sequence-id` selects that configured sequence;
if the heading already has a TODO keyword, it must belong to the selected
sequence. If the heading has no TODO keyword and no ID is supplied, cycling
selects a sequence automatically only when exactly one is configured. Otherwise,
pass its ID as a string, such as `:sequence-id "FlowId"`.
Both functions return two values: an `edit-result` and a `transition-result`.
The `edit-result` contains the updated document; the `transition-result`
reports the previous and new states, log events, repeater rescheduling, and any
`CLOSED` timestamp. These changes are already present in the updated document.

Both functions require an explicit `:at` value. Omitting it signals
`org-edit-error` with reason `:invalid-value`; invalid or ambiguous time input
signals `org-time-error`. `:at` accepts an `org-time-value`, a string accepted
by `make-org-time-value`, or a `cl-date-kit` local date or local date-time.
Date-time repeaters require `:time-context` to apply its time zone and
daylight-saving policy. The example's offset-free datetime has no repeater, so
the transition needs no time zone and stores the value as given.

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
            (format nil "#+TODO: TODO | DONE~%~%* TODO Prepare release~%")))
         (heading (first-node-of-type document :headline)))
    (multiple-value-bind (edit transition)
        (org-kit:transition-heading-todo
         document heading "DONE" :at "2025-01-01T09:00:00")
      (list
       (org-kit:write-org (org-kit:edit-result-updated-document edit))
       (org-kit:transition-result-previous-state transition)
       (org-kit:transition-result-new-state transition)
       (org-kit:transition-result-log-events transition)))))
~~~

### Logging settings

A transition records a `CLOSED` timestamp, a closing note, state-change log
lines, and `LAST_REPEAT` according to Org's logging settings. When the heading
has or inherits a `LOGGING` property, including one from a
`#+PROPERTY: LOGGING` line, that property alone decides. As in Org, it turns
off every logging setting it does not name. Its words can be `logdone`,
`lognotedone`, `nologdone`, `logrepeat`, `lognoterepeat`, `nologrepeat`, and
TODO keyword specs such as `WAIT(w@/!)`, which replace the markers on the
`#+TODO` line; `nil` turns all logging off.

In these markers, `!` records a state change and `@` requests a note; `/`
separates the markers for unfinished and done states.

Without `LOGGING`, state-change lines follow the `#+TODO` markers. Done and
repeat logging each come from the `:log-done` or `:log-repeat` argument given
to `make-org-workflow-options`, and otherwise from the last `logdone`-family
word of `#+STARTUP` (see `startup-options` in the
[node model](../reference/node-model.md)). Pass the resulting value as
`:workflow-options` to `transition-heading-todo` or `cycle-heading-todo`.
`LOG_DONE` and `LOG_REPEAT` properties have no effect.

As in Org, removing a TODO keyword writes no state-change line, and a
`lognotedone` closing note is written only when no state-change line is.

An unknown state, invalid heading, or conflicting edit signals
`org-edit-error`. For `++` repeaters, which keep the original interval cadence
and skip missed occurrences, a DONE transition advances the heading's repeating
timestamps until they fall after the supplied `:at` time. The document's
`:max-future-repeats` limit caps these interval advances and defaults to 10,000.
Set it with `:max-future-repeats` in a `make-limits` value passed as `:limits`
to `parse-org`. Exceeding it signals `org-resource-limit-error`.

Changing a heading to a DONE state is refused with `org-edit-error` reason
`:blocked` in any of these cases:

- A descendant TODO heading is unfinished.
- The target is in an ordered sibling sequence and an earlier sibling in
  document order has an unfinished TODO state.
- An unfinished TODO ancestor is blocked by an earlier unfinished TODO sibling
  in an ordered parent sequence. For example, if `* Project` is ordered, an
  unfinished `** TODO Prep` before `** TODO A` blocks a target nested under A.

Put `:ORDERED: t` in the parent heading's `:PROPERTIES:` drawer to mark its child
sequence as ordered. Put `:NOBLOCKING: t` in the target heading's drawer to skip
these dependency checks. Unchecked checkboxes do not block TODO transitions.

The built-in `BLOCKED` query uses the same descendant, ordered-sibling, and
ancestor checks as the DONE transition, including the `:NOBLOCKING:` exception.
Pass a one-argument function as `:blocked-provider` to `make-query-context`; it
receives the headline node. When present, `query-list` calls the provider first.
Any non-`nil` return value is exposed as `BLOCKED` unchanged and skips the
built-in checks.
When the provider is absent or returns `nil`, the built-in rules return `"t"`
only for an unfinished TODO heading that is blocked; otherwise `BLOCKED` is
missing. The example uses `query-list`'s default context, which runs the built-in
rules without a provider:

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil "#+TODO: TODO | DONE~%* TODO Parent~%** TODO Child~%")))
       (matches
         (org-kit:query-list document '(:property "BLOCKED" := "t"))))
  (format t "~D~%"
          (length matches)))
~~~

The result is `1`: the parent headline is blocked by its unfinished child.

## Build a TODO list and agenda

The released 0.1.0 agenda and query APIs include `agenda` for date-range
agendas, `agenda-todo-list`, `agenda-match`, and `query-match-now`. This guide
also covers the TODO editing, priority, and transition APIs from the released
0.1.0 package. The agenda APIs added to this source tree after the 0.2.0
release are not part of the released package. They include search and
stuck-project views, sorting and category support, time grids, custom agenda
views, and agenda text, Org, and edit output. To run those examples, use the
working-checkout setup in [Development](../project/development.md), not the
release-tag setup in [Getting Started](../getting-started.md).

The date-range, TODO-list, and match calls below require an explicit time
context. This makes the reference time, time zone, and daylight-saving-time
policy part of the call instead of using the machine's current clock. Agenda
dates are date-only `org-time-value` objects, created with
`make-org-time-value`.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil
                  "#+TODO: TODO | DONE~%~%* TODO Publish 0.1.0~%DEADLINE: <2025-01-02 Thu>~%")))
       (match "TODO=\"TODO\"")
       (time-context
         (org-kit:make-time-context
          :reference-time (org-kit:make-org-time-value "2025-01-01T12:00:00")
          :zone (org-kit:make-fixed-time-zone 0)))
       (date (org-kit:make-org-time-value "2025-01-02"))
       (todo-result
         (org-kit:agenda-todo-list (list document) :match match
                                   :time-context time-context))
       (day-result
         (org-kit:agenda (list document) date date
                        :time-context time-context))
       (match-result
         (org-kit:agenda-match (list document) match
                               :time-context time-context)))
  (list
   (mapcar #'org-kit:agenda-entry-headline
           (org-kit:agenda-result-entries todo-result))
   (mapcar (lambda (entry)
             (list (org-kit:agenda-entry-headline entry)
                   (org-kit:agenda-entry-kind entry)
                   (org-kit:org-time-value-iso8601
                    (org-kit:agenda-entry-date entry))))
           (org-kit:agenda-result-entries day-result))
   (mapcar (lambda (entry)
             (list (org-kit:node-property
                    (org-kit:agenda-match-entry-node entry) :raw-value)
                   (org-kit:agenda-match-entry-line entry)))
           match-result)))
~~~

`agenda-todo-list` returns an `agenda-result` containing TODO headlines whose
states are not configured as done, in document order. The example's day rows
include the raw title, source kind, and agenda date; here the deadline row is
`("Publish 0.1.0" :DEADLINE "2025-01-02")`. Its optional `:match`
argument filters those headlines with Org match syntax. `agenda` returns an
`agenda-result` with entries for the inclusive start and end dates. Its call
options are `:warning-days` (14 days before a deadline by default),
`:scheduled-delay` (zero days by default), and `:future-repeats` (default
`:all`).
`:warning-days` sets the default lead time for deadline prewarnings. A warning
amount and unit on the deadline timestamp override it. Once the warning date has
arrived and the deadline is still ahead, an agenda row is dated on the reference
date if that date is in range.
`:scheduled-delay` is a nonnegative integer day count. An explicit delay on the
`SCHEDULED` timestamp, such as `--2d` (add two days), overrides that option.
The visibility date is the source scheduled date plus the effective delay.
With zero delay, the first row is dated on the source scheduled date. With a
positive delay, that row is omitted; after the source date has passed and the
visibility date arrives, `agenda` adds a reminder dated on the reference date
if that date is in range. A task scheduled for March 3 with `--2d` has a
visibility date of March 5: it has no row with a March 4 reference date, and its
reminder row is dated March 5 when the reference date reaches March 5. Repeat
rows appear on their occurrence dates once visible.
`agenda-entry-scheduled-delay-state` returns `:delayed` for a reminder or repeat
row when the original `SCHEDULED` date precedes the reference date and the
effective delay is positive, including a repeat row whose occurrence date is
still in the future. The flag uses the source `SCHEDULED` date, not the repeat
occurrence date. This accessor and flag are cl-org-kit metadata.

`:future-repeats` can be `:all`, `:next`, or `:none`. For repeating scheduled or
deadline entries, this setting controls repeat occurrences; the original row
can still appear when its date is in range. `:all` includes repeat occurrences
through the requested inclusive date range. If the original date is on or
before the reference date, `:next` adds at most one repeat occurrence after the
reference date. If the original date is already in the future, `:next` adds no
repeat occurrence. `:none` adds no repeat occurrences. These are call settings,
not document resource limits.

Use `agenda-result-entries` to read rows and `agenda-entry-headline`,
`agenda-entry-kind`, `agenda-entry-date`, `agenda-entry-heading-path`, and the
other `agenda-entry-*` accessors to inspect them. `agenda-todo-list` has
signature
`(agenda-todo-list documents &key match time-context sorting-strategy user-defined-sorter)`.
The matcher signature is
`(agenda-match documents match &key time-context sorting-strategy user-defined-sorter)`.
It returns matching headlines in document order by default as
`agenda-match-entry` values, with document, node, heading path, and source line
information. Read those fields with
`agenda-match-entry-document`, `agenda-match-entry-node`,
`agenda-match-entry-heading-path`, and `agenda-match-entry-line`.
`agenda-entry-headline`
returns a raw headline title string, while `agenda-match-entry-node` returns a
headline node. In the example, `node-property` with `:raw-value` reads that
node's raw title, and `agenda-match-entry-line` returns its source line.

`agenda` has signature
`(agenda documents start-date end-date &key time-context (warning-days 14) (scheduled-delay 0) (future-repeats :all) sorting-strategy user-defined-sorter)`.

`query-match-now` has the signature
`(query-match-now document match &key time-context context)` and returns
matching headlines in document order. It matches an Org expression against one
document outside an agenda call. It requires `:time-context` and evaluates
relative dates at that context's reference instant in its configured time zone.
The optional `:context` is a `query-context` for query settings such as a
`BLOCKED` provider; omit it to use the default context.

### Diary expressions

`agenda` evaluates standalone diary sexps and diary timestamps for each date in
the requested range. The built-in forms are `diary-anniversary`, `diary-block`,
`diary-cyclic`, `diary-float`, and `diary-date`. Their date arguments follow
Emacs's default `calendar-date-style`, `:american`, so direct forms use
month, day, and year order. Other forms are passed unchanged as complete
parenthesized Emacs Lisp expressions to the `:evaluator`/`:emacs-lisp` callback
in the `:callback-registry` supplied to `agenda`. Register it with
`(register-callback registry :evaluator :emacs-lisp function)`. Here
`:evaluator` is the callback kind and `:emacs-lisp` is its name. The function
receives the expression and a context plist. Structured `query-list` uses the
same callback contract for `(:diary-match)`.

The context's `:entry` value is the diary entry text (empty for a diary
timestamp), and `:date` is the candidate date as
a Gregorian `(month day year)` list. If the callback is not registered,
`agenda` signals `org-elisp-evaluator-error` with reason `:unregistered`.
The library bounds expression length and nesting, and rejects `#.` and `#S`
reader forms and package-qualified symbols before invoking the callback.

Structured `query-list` also accepts `(:diary-match)` with `:scope :nodes` and
an explicit `:reference-time`. It returns matching standalone diary sexp and
diary timestamp nodes for that date. Its optional `:callback-registry` defaults
to an empty registry; an unknown diary form signals
`org-elisp-evaluator-error` with reason `:unregistered` unless an evaluator is
registered with `(register-callback registry :evaluator :emacs-lisp function)`
and that registry is passed as `:callback-registry`.

Agenda rows for a scheduled habit also expose `agenda-entry-habit-consistency`.
Each scheduled row for a habit carries one list spanning the full agenda date
range. `agenda-entry-habit-p` identifies such a row in
`agenda-result-entries`; `agenda-entry-habit-consistency` returns one
`(:date ... :status ...)` plist for each date in the inclusive agenda range. A
heading is a habit when its `STYLE` property is `habit`.
Dates use date-only `org-time-value` objects. A TODO transition contributes a
completion date only when the habit heading has a timestamped DONE state-change
entry, such as `- State "DONE" from "TODO" [2025-01-05 Sun]`. When the heading
is currently done, its `CLOSED` timestamp also contributes a date. These dates
have status `:completed`; child-heading DONE logs and their `CLOSED` timestamps
do not count. Dates before the first due date and through each due date are
`:within-schedule`; later dates are `:late` until the next due date. The current
`SCHEDULED` timestamp is the due date for its pending occurrence after the
latest completion. Earlier due dates are reconstructed from completion dates
using the repeater mode:

- `.+` sets the next due date one interval after each earlier completion.
- `+` keeps the original cadence. For each DONE log, count the later DONE logs
  and subtract that many intervals from the current `SCHEDULED` date. With a
  weekly repeater scheduled for February 5 and DONE logs on January 15 and
  February 1, the January 15 completion has one later DONE log and is due
  January 29; the February 1 completion has none and is due February 5.
- `++` keeps the original cadence. For each completion, use the first cadence
  occurrence strictly after that completion, skipping occurrences it passed.
  With the same weekly schedule and DONE dates, the January 15 completion is
  due January 22; the February 1 completion is due February 5, skipping January
  29 because it is before that completion.

For a daily `.+1d` habit scheduled for January 10 with heading-local DONE logs on
January 5 and January 9, those completions set the next due dates to January 6
and January 10. Across January 5–10, the statuses are `:completed` on January 5
and 9, `:within-schedule` on January 6 and 10, and `:late` on January 7 and 8.

For reconstructed earlier due dates, month intervals use 30.4 days per month
and year intervals use 365.25 days per year; the day count is rounded down.
These approximations do not affect the currently scheduled occurrence. Status
is calculated for every date in the requested agenda range, including dates
after the context's reference date. Rows for non-habits return `nil`. A habit
must have a TODO keyword and an active `SCHEDULED` timestamp with a repeater,
such as `<2025-01-02 Thu .+1d>`. An inactive `[2025-01-02 Thu .+1d]` timestamp
does not qualify. Otherwise `agenda` signals `org-agenda-error` with reason
`:habit-not-todo`, `:habit-without-schedule`, or `:habit-without-repeater`.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil
                  "#+TODO: TODO | DONE~%~%* TODO Drink water~%SCHEDULED: <2025-01-02 Thu .+1d>~%:PROPERTIES:~%:STYLE: habit~%:END:~%")))
       (time-context
         (org-kit:make-time-context
          :reference-time (org-kit:make-org-time-value "2025-01-03T12:00:00")
          :zone (org-kit:make-fixed-time-zone 0)))
       (result
         (org-kit:agenda (list document)
                         (org-kit:make-org-time-value "2025-01-01")
                         (org-kit:make-org-time-value "2025-01-04")
                         :time-context time-context)))
  (org-kit:agenda-entry-habit-consistency
   (find-if #'org-kit:agenda-entry-habit-p
            (org-kit:agenda-result-entries result))))
~~~

Invalid agenda options or document lists signal `org-agenda-error`. Invalid
dates or time contexts signal `org-time-error`, malformed match syntax signals
`org-query-error`, and an exceeded range, repeat, or entry ceiling signals
`org-resource-limit-error`. The default limits are 3,660 inclusive days,
1,000,000 generated entries, and 10,000 future repeats. For each agenda limit, a
call over multiple documents uses the lowest value among those documents; a
document without an override contributes the default. Pass a limits object to
`parse-org` to change a ceiling, for example
`:limits (org-kit:make-limits :max-agenda-range-days 90 :max-future-repeats 500)`.

### Search headings and find stuck projects

`agenda-search-view` has signature
`(agenda-search-view documents query &key sorting-strategy user-defined-sorter time-context)`.
It searches each headline's subtree text, including the headline itself and
descendant content, and returns matching headlines as `agenda-match-entry`
values. The first non-whitespace character selects the mode. If it is not `+`,
`-`, or `{`, the trimmed query is searched as one case-insensitive phrase.
Spaces in the phrase match any run of whitespace in the subtree text. Quotes
have no grouping role in this mode and are searched literally. Otherwise, the
query uses Boolean mode. Separate Boolean snippets with whitespace. A
double-quoted snippet keeps a multiword text phrase together; braces keep a
regular expression together, including whitespace in its pattern. In Boolean
mode, `+` marks a positive snippet and `-` marks a veto. An unprefixed snippet
is also positive, and every positive snippet is required to match. Any matching
negative snippet rejects the headline. `{regexp}` uses Emacs
regular-expression syntax and matches case-insensitively. A query with only veto
snippets matches headlines whose subtrees match none of them. In the example,
`release` and `{notes}` must match, while `draft` must not.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil "#+TODO: TODO | DONE~%~%* TODO Release notes~%")))
       (entries
         (org-kit:agenda-search-view
          (list document) "+release -draft {notes}")))
  (mapcar #'org-kit:agenda-match-entry-line entries))
~~~

`agenda-stuck-projects` has signature
`(agenda-stuck-projects documents stuck-projects &key time-context sorting-strategy user-defined-sorter)`.
It returns remaining candidates as a list of `agenda-match-entry` values. It
takes an Org matcher and three veto criteria as a four-element list:
`(matcher todo-veto tags-veto subtree-regexp)`. The matcher selects candidate
headings first. The vetoes inspect each candidate's whole subtree, including its
own heading. A candidate is removed if any heading in that subtree has an
unfinished TODO keyword listed in `todo-veto` or an explicit tag listed in
`tags-veto`, or if `subtree-regexp` matches the whole subtree text. The TODO
veto `"*"` matches any unfinished TODO keyword. The tag veto `"*"` matches any
explicit tag on the candidate or one of its descendants. A tag check uses only
tags written on each heading; inherited tags do not trigger a veto. An empty or
`nil` regular expression disables that veto. At least one of the three veto
criteria must be set. Each nested candidate is checked independently against
its own subtree, so a descendant that vetoes its parent may still be returned
as a candidate itself.

For example, this call keeps a project whose only child is already done:

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil
                  "#+TODO: TODO NEXT | DONE~%~%* TODO Project~%** DONE Completed work~%")))
       (time-context
         (org-kit:make-time-context
          :reference-time (org-kit:make-org-time-value "2025-01-01T12:00:00")
          :zone (org-kit:make-fixed-time-zone 0))))
  (org-kit:agenda-stuck-projects
   (list document) '("TODO=\"TODO\"" ("NEXT") nil nil)
   :time-context time-context))
~~~

The matcher may contain relative date terms, so `agenda-stuck-projects` needs
the same explicit `:time-context` used by `agenda`. Its `subtree-regexp` veto is
case-sensitive; search-view `{regexp}` terms are case-insensitive. Search and
stuck-project views accept `:sorting-strategy` and `:user-defined-sorter`; their
default order is document order.

Search strings obey `:max-input-length` (67,108,864 characters by default).
Stuck-project match syntax uses each source document's input-length limit.
Regex patterns obey `:max-search-regex-length` (4,096 characters by default).
`agenda-search-view` counts phrase and regex matching against
`:max-search-work` (67,108,864 work units by default). Phrase and regex work is
cumulative per document. `agenda-stuck-projects` applies the per-document limit
to its subtree regex checks. Search and stuck-project results obey
`:max-agenda-entries` (1,000,000 by default). With multiple documents,
regex-length and entry-count limits use the lowest corresponding value. Search
query length also uses the lowest input-length limit.

### Sort agenda views

Pass `:sorting-strategy` as an ordered proper list of supported keyword criteria to `agenda`,
`agenda-todo-list`, `agenda-match`, `agenda-search-view`, or
`agenda-stuck-projects`. A `nil` strategy keeps the view's default order. A
non-`nil` strategy stably sorts a copy, so equal entries keep their prior
relative order. Criteria are applied in list order. An improper list or an
unsupported key signals `org-agenda-error` with reason `:invalid-option`.
Custom view definitions accept the same keys in their `:options` plist.

The supported criteria are:

- Time and timestamp: `:time-up`, `:time-down`, `:timestamp-up`,
  `:timestamp-down`, `:scheduled-up`, `:scheduled-down`, `:deadline-up`,
  `:deadline-down`, `:ts-up`, `:ts-down`, `:tsia-up`, and `:tsia-down`.
- Category and tags: `:category-keep`, `:category-up`, `:category-down`,
  `:tag-up`, and `:tag-down`.
- Heading metadata: `:priority-up`, `:priority-down`, `:urgency-up`,
  `:urgency-down`, `:todo-state-up`, `:todo-state-down`, `:effort-up`,
  `:effort-down`, `:habit-up`, `:habit-down`, `:alpha-up`, and `:alpha-down`.
- Caller comparator: `:user-defined-up` and `:user-defined-down`.

`:time-up` and `:time-down` compare time of day, ignoring the date. Agenda rows
use their row timestamp; match and search entries use the first standalone clock
time in the headline title. `:timestamp-up` and `:timestamp-down` compare dates:
agenda rows use their row timestamp, while match and search entries use the
heading's first available timestamp, preferring `SCHEDULED`, `DEADLINE`, active,
then inactive. `:scheduled-up` and `:scheduled-down` compare only the heading's
`SCHEDULED` timestamp; among agenda rows, only scheduled rows have a value.
`:ts-up` and `:ts-down` sort by active timestamps on the heading;
`:tsia-up` and `:tsia-down` sort by inactive timestamps.

`:category-keep` stops the entire comparison for a pair with different
categories. Later criteria are skipped for that pair, preserving its prior
relative order.

For example, sort a date-range agenda by category and then by deadline:

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil
                  "#+TODO: TODO | DONE~%~%* TODO Publish release~%DEADLINE: <2025-01-02 Thu>~%")))
       (time-context
         (org-kit:make-time-context
          :reference-time (org-kit:make-org-time-value "2025-01-01T12:00:00")
          :zone (org-kit:make-fixed-time-zone 0)))
       (date (org-kit:make-org-time-value "2025-01-02"))
       (result
         (org-kit:agenda (list document) date date
                         :time-context time-context
                         :sorting-strategy '(:category-up :deadline-up))))
  (mapcar #'org-kit:agenda-entry-headline
          (org-kit:agenda-result-entries result)))
~~~

The caller comparator takes two agenda or match entries. Return `-1` when the
first argument sorts before the second, `1` when it sorts after, or `nil` when
they compare equal. Pass it as `:user-defined-sorter` when the strategy includes
`:user-defined-up` or `:user-defined-down`; the down variant reverses the
arguments. A missing comparator or a return value other than `-1`, `1`, or
`nil` signals `org-agenda-error` with reason `:invalid-option`. Urgency criteria
require an explicit `:time-context`.

### Categories and time grids

Agenda entries expose `agenda-entry-category`. It follows Org category
resolution: an effective `CATEGORY` property, the document's `#+CATEGORY`, the
source file's basename, then `"???"` when none is available.

`agenda-time-grid` has signature
`(agenda-time-grid documents start-date end-date &key time-context times remove-match)`.
It returns rows for explicit clock ticks over an inclusive range of date-only
`org-time-value` values. By default it uses `'(800 1000 1200 1400 1600 1800 2000)`.
Pass `:times` to choose a different list of `HHMM` integers; `2400` is also
accepted. The function keeps the supplied order and duplicates. Its result uses
`agenda-time-grid-row-date`, `agenda-time-grid-row-time`, and
`agenda-time-grid-row-current-p` accessors.

Pass `:time-context` to add a current-time marker using the local date and time
at its reference instant; the function does not read the system clock. The
marker is a separate row after that date's configured ticks. `:remove-match t`
removes a configured tick only when both its date and `HHMM` match the start
time of a generated agenda row with a time-of-day timestamp. Date-only and
untimed rows do not match. This option requires `:time-context`; it does not
remove the current-time marker.

The grid uses the `:max-agenda-range-days` and `:max-agenda-entries` limits
described above for its inclusive date span and generated rows.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil
                  "#+TODO: TODO | DONE~%~%* TODO Meeting~%SCHEDULED: <2025-01-01 Wed 09:00>~%")))
       (start (org-kit:make-org-time-value "2025-01-01"))
       (end (org-kit:make-org-time-value "2025-01-02"))
       (rows (org-kit:agenda-time-grid
              (list document) start end :times '(900 1200 1500))))
  (mapcar (lambda (row)
            (list (org-kit:org-time-value-iso8601
                   (org-kit:agenda-time-grid-row-date row))
                  (org-kit:agenda-time-grid-row-time row)
                  (org-kit:agenda-time-grid-row-current-p row)))
          rows))
~~~

### Define custom agenda views

`agenda-custom` has signature
`(agenda-custom documents definitions &key time-context callbacks)`. It
evaluates definitions in order and returns one `agenda-custom-view` for each
definition. Every definition is a property list
with a unique nonempty `:key` (a string, symbol, or character) and a `:type` of
`:agenda`, `:todo`, `:match`, or `:search`. `:description` is optional. An
`:agenda` definition needs
`:start-date` and `:end-date`; `:match` and `:search` definitions need their
corresponding `:match` or `:search` string. A `:todo` definition can omit
`:match` to include every unfinished TODO heading.

Use `:options` for settings accepted by the selected view. `:agenda` accepts
`:warning-days`, `:scheduled-delay`, and `:future-repeats`; all four view types
accept `:sorting-strategy` and `:user-defined-sorter`. `agenda-custom-view-key`,
`agenda-custom-view-description`, `agenda-custom-view-type`, and
`agenda-custom-view-result` read the returned view. Agenda and TODO definitions
contain an `agenda-result`; match and search definitions contain a list of
`agenda-match-entry` values.

The number of definitions is limited by `:max-agenda-entries` across the
source documents.

Agenda, TODO, and match definitions require the explicit `:time-context` passed
to `agenda-custom`. A search definition uses that context when its sorting
strategy contains an urgency criterion. A definition may wrap its `:match` or
`:search` value as `(:elisp expression)` to evaluate it through
`evaluate-elisp`; pass a `callback-registry` as `:callbacks` when that
expression needs registered callbacks. The evaluated expression must return a
string. `evaluate-elisp` passes the expression unchanged to the registered
Emacs Lisp evaluator; the library does not parse or evaluate it. For an evaluator
that reads Emacs Lisp source, `(:elisp "\"+release {notes}\"")` evaluates to
the search string `+release {notes}`. See
[Callbacks and Write Plans](callbacks-and-write-plans.md) for evaluator
registration.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil "#+TODO: TODO | DONE~%~%* TODO Draft release notes~%")))
       (time-context
         (org-kit:make-time-context
          :reference-time (org-kit:make-org-time-value "2025-01-01T12:00:00")
          :zone (org-kit:make-fixed-time-zone 0)))
       (views
         (org-kit:agenda-custom
          (list document)
          '((:key "open" :description "Open tasks" :type :todo)
            (:key "release" :type :search :search "+release {notes}"
             :options (:sorting-strategy (:alpha-up))))
          :time-context time-context)))
  (mapcar (lambda (view)
            (list (org-kit:agenda-custom-view-key view)
                  (org-kit:agenda-format-text view)))
          views))
~~~

### Format or copy agenda results

`agenda-format-text` has signature `(agenda-format-text view)`. It returns
stable plain text for an agenda result, a list of match entries, or a custom
view. It follows Org 9.8.10's default agenda buffer layout: date headings align
weekday and date, rows prefix the category and available time or status
information. When a row has tags, the formatter combines its TODO keyword and
headline title into one body, separating them with a space when both exist. If
the body is shorter than 50 characters, padding brings it to that width before
the tag suffix; otherwise one separator space remains. Category and
time/status prefixes are outside the body, so they shift the tag suffix on the
full line. Match rows have no time prefix.

`agenda-to-org` has signature `(agenda-to-org view)` and returns an Org document
containing copies of selected headings and their section bodies. Each copy runs
through the line before its first child heading, if present, so child subtrees
are excluded. When selected rows yield identical source fragments, the fragment
is copied once before heading levels are normalized. Every selected heading is
written at level 1, so selected parent and child headings remain separate. The
fragments are concatenated without blank lines. Text outside the selected
heading sections is omitted; keyword lines within a copied section are retained.
Serialize the returned document with `write-org` when you need a string.

`agenda-write-into` has signature
`(agenda-write-into document view start end &key (format :text))`. It returns an
`edit-result` that replaces the half-open, zero-based character range `[start,
end)` in the document's source with `:text` output by default or `:org` output
when requested.
Use `edit-result-updated-document` to read the changed document. These APIs
return strings, documents, or edits; they do not write files.

Formatting and Org conversion enforce the contributing documents'
`:max-agenda-entries`, `:max-output-length`, and `:max-output-octets` limits.
The default output ceilings are 67,108,864 characters and 268,435,456 UTF-8
bytes.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil
                  "#+TODO: TODO | DONE~%~%* TODO Publish release~%DEADLINE: <2025-01-02 Thu>~%")))
       (time-context
         (org-kit:make-time-context
          :reference-time (org-kit:make-org-time-value "2025-01-01T12:00:00")
          :zone (org-kit:make-fixed-time-zone 0)))
       (date (org-kit:make-org-time-value "2025-01-02"))
       (result
         (org-kit:agenda (list document) date date
                         :time-context time-context))
       (text (org-kit:agenda-format-text result))
       (org-text
         (org-kit:write-org (org-kit:agenda-to-org result)
                            :output-type :string)))
  (list text org-text))
~~~

### Apply bulk actions

`agenda-bulk-action` applies one action to the headlines in an agenda result,
custom view, or list of agenda entries. It returns an alist of original
documents paired with their combined `edit-result`. The function stages edits
in memory; callers can inspect each updated document or build a write plan.

Use `:todo` with `:state` and explicit `:at` values to transition selected
headlines. Bulk transitions suppress workflow logging and, like Org 9.8.10's
agenda bulk action, ignore TODO dependency blockers. A date-time repeater also
requires `:time-context`. Use `:add-tag` with `:tag` to add a local tag, or
`:schedule` with `:schedule-date` to set a date-only schedule. Pass `nil` as
the schedule date to remove `SCHEDULED`. Replacing a schedule preserves its
repeater, warning delay, and range length.

Use `:refile` with `:target-document` and an optional `:target-heading` to move
selected subtrees. `:archive` moves them below an `Archive` sibling in the same
document; pass a `:target-document` and optional `:target-heading` to archive
into another in-memory document. Archive operations require an explicit
`:time-context` to set `ARCHIVE_TIME`. A selected parent covers selected
descendants for refile and archive. Repeated agenda rows for the same headline
are applied once. The `:max-agenda-bulk-actions` limit defaults to 100,000
selected rows per call.

Edits are staged across documents. If applying one selection fails, the call
signals `org-agenda-error` with reason `:bulk-action-failed`; its context
identifies the document and headline path. The call returns results only after
every selected action succeeds, so earlier staged edits are not returned when
a later selection fails.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil
                  "#+TODO: TODO | DONE~%~%* TODO Publish release~%DEADLINE: <2025-01-02 Thu>~%")))
       (start-date (org-kit:make-org-time-value "2025-01-02"))
       (end-date start-date)
       (time-context
         (org-kit:make-time-context
          :reference-time
          (org-kit:make-org-time-value "2025-01-01T12:00:00")
          :zone (org-kit:make-fixed-time-zone 0)))
       (view (org-kit:agenda (list document) start-date end-date
                             :time-context time-context))
       (results
         (org-kit:agenda-bulk-action
          view :schedule :schedule-date (org-kit:make-org-time-value "2025-01-03")))
       (edit (cdr (assoc document results))))
  (org-kit:document-text (org-kit:edit-result-updated-document edit)))
~~~

See [Clock](clock.md) for clock aggregation and
[API Stability](../reference/api-stability.md), which defines the released
0.2.0 API scope and identifies agenda APIs added after that release.

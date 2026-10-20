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
document; the original document remains unchanged. The edit reparses the
document, so find the headline again in the returned document with `map-nodes`
before making another edit, as shown in the example below.

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
each edit reparses it. Find the target headline again in the returned document
with `map-nodes` (as in the first example above) before making another edit; a
headline node from the original document belongs to its old parse. To wrap from
A to C, call
`(org-kit:adjust-heading-priority document heading :up)`; this removes `[#A]` and
leaves the heading unmarked. On the updated document and heading, call
`(org-kit:adjust-heading-priority updated-document updated-heading :up :previous-direction :up)`
to set `[#C]`. `:previous-direction` tells the second call that the first call
cleared the marker while moving in the same direction.
Without a matching
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

Without `LOGGING`, state-change lines follow the `#+TODO` markers. Done and
repeat logging each come from the `:log-done` or `:log-repeat` argument given
to `make-org-workflow-options`, and otherwise from the last `logdone`-family
word of `#+STARTUP` (see `startup-options` in the
[node model](../reference/node-model.md)). `LOG_DONE` and `LOG_REPEAT`
properties have no effect.

As in Org, removing a TODO keyword writes no state-change line, and a
`lognotedone` closing note is written only when no state-change line is.

An unknown state, invalid heading, or conflicting edit signals
`org-edit-error`. For `++` repeaters, a DONE transition advances the heading's
repeating timestamps by intervals until they fall after the supplied `:at`
time. The document's
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
  The sibling relation is between Prep and A, whose parent is Project.

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

Agenda calls require an explicit time context. This makes the reference time,
time zone, and daylight-saving-time policy part of the call instead of using
the machine's current clock. Agenda dates are date-only `org-time-value`
objects, created with `make-org-time-value`.

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
The visibility date is the source scheduled date plus the effective delay. The
source date is the delay base; a reminder row is dated on the reference date,
and a repeat row is dated on its occurrence date.
With zero delay, the first row is dated on the source scheduled date. With a
positive delay, that row is omitted; after the source date is overdue and the
visibility date arrives, `agenda` adds a reminder dated on the reference date
if that date is in range. A task scheduled for March 3 with `--2d` has a
visibility date of March 5; with a March 4 reference date it has no row yet,
and at a March 5 reference date its reminder row is dated March 5. A repeat row
appears on its occurrence date once that date is visible.
`agenda-entry-scheduled-delay-state` returns
`:delayed` for a reminder or repeat row when the base `SCHEDULED` date is before
the reference date and the effective delay is positive. The flag follows that
base date, so a repeat row can be delayed even when its occurrence date is in
the future.

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
other `agenda-entry-*` accessors to inspect them. The matcher signature is
`(agenda-match documents match &key time-context)`. It returns matching
headlines in document order as `agenda-match-entry` values, with document,
node, heading path, and source line information. Read those fields with
`agenda-match-entry-document`, `agenda-match-entry-node`,
`agenda-match-entry-heading-path`, and `agenda-match-entry-line`. It requires
the same explicit time context as the agenda calls. `agenda-entry-headline`
returns a raw headline title string, while `agenda-match-entry-node` returns a
headline node. In the example, `node-property` with `:raw-value` reads that
node's raw title, and `agenda-match-entry-line` returns its source line.

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
and that registry is passed as `:callback-registry`. `query-match-now` remains
an Org headline matcher. Relative query dates use its explicit `:time-context`
reference time rather than the machine clock.

Agenda rows for a scheduled habit also expose `agenda-entry-habit-consistency`.
Each scheduled row for a habit carries one list spanning the full agenda date
range. `agenda-entry-habit-p` identifies such a row in
`agenda-result-entries`; `agenda-entry-habit-consistency` returns one
`(:date ... :status ...)` plist for each date in the inclusive agenda range. A
heading is a habit when its `STYLE` property is `habit`.
Dates use date-only `org-time-value` objects. Existing DONE transition log
entries on the habit heading contribute completion dates; transitions without
a DONE log entry do not add one. When the heading is currently done, its
`CLOSED` timestamp also contributes a date. These dates have status
`:completed`.
Child-heading DONE logs and their `CLOSED` timestamps do not count. Dates before
the first due date and through each due date are `:within-schedule`; later
dates are `:late` until the next due date. The current `SCHEDULED` timestamp is
the due date for its pending occurrence after the latest completion. Earlier
due dates are reconstructed from completion dates using the repeater mode:

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

See [Clock](clock.md) for clock aggregation and
[API Stability](../reference/api-stability.md) for the 0.1.0 public API scope.

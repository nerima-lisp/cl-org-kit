# TODO and Agenda

TODO keywords and planning timestamps are stored on parsed headlines.

## Set a TODO keyword

`set-heading-todo` sets the keyword supplied by the caller. The keyword must be
configured through a `#+TODO` line, the `:todo-keywords` argument, or
`:todo-keywords` in the `:settings` plist passed to `parse-org`. Pass `nil` to
remove a TODO keyword. The function returns the updated document; the original
document remains unchanged.

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
such as `#+PRIORITIES: 1 10 5`. The endpoints and default must use the same kind,
and the default must lie between the endpoints. Only the first three values are
used; directives with fewer than three values are ignored. Without the
directive, Org's A, C, and B defaults apply.

`set-heading-priority` accepts an uppercase letter or a numeric priority and
rejects values outside the configured range. Pass `nil` to clear the priority.
`adjust-heading-priority` moves one step with `:up` or `:down`. The first
adjustment of an unmarked heading assigns the configured default. Moving past a
range endpoint clears the priority cookie. To wrap on the next call, pass the
direction that cleared the cookie as `:previous-direction` and use that same
direction again; the unset priority then starts at the opposite endpoint. An
unset priority without a matching `:previous-direction` starts at the default.
Invalid directives, directions, or values signal `org-edit-error` with reason
`:invalid-value`. A numeric `PRIORITY` query value is returned as decimal text.

## Transition a TODO state

`transition-heading-todo` sets a heading to a configured state, while
`cycle-heading-todo` advances or reverses through a configured TODO sequence.
Pass `:direction :next` to advance or `:direction :previous` to reverse with
`cycle-heading-todo`. If the heading has no TODO state and more than one active
sequence is configured, pass that sequence's ID as `:sequence-id`.
Both functions return two values: an `edit-result` and a `transition-result`.
The `edit-result` contains the updated document; the `transition-result`
describes the previous and new states, log events, repeater rescheduling, and
any `CLOSED` timestamp.

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
`org-edit-error`; invalid or ambiguous time input signals `org-time-error`.
When a repeater transition catches up missed occurrences, expansion is bounded
by the document's `:max-future-repeats` limit, which defaults to 10,000. An
exceeded limit signals `org-resource-limit-error`.

Changing a heading to a DONE state is refused with `org-edit-error` reason
`:blocked` when any TODO heading in its subtree is unfinished, or when an
earlier unfinished TODO sibling blocks it under an `ORDERED` parent or
ancestor. Put `:ORDERED: t` in the parent heading's `:PROPERTIES:` drawer to mark
an ordered sequence. Put `:NOBLOCKING: t` in the target heading's drawer to skip
its dependency checks. Unchecked checkboxes do not block TODO transitions.
Querying the special `BLOCKED` property combines these rules with an optional
provider. Pass a one-argument function as `:blocked-provider` to
`make-query-context`; it receives the headline node and returns `nil` or the
provider value to expose as `BLOCKED`. A non-`nil` provider value is preserved;
otherwise a built-in blocked result is returned as `"t"`.

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
   (mapcar #'org-kit:agenda-entry-headline
           (org-kit:agenda-result-entries day-result))
   (mapcar (lambda (entry)
             (list (org-kit:node-property
                    (org-kit:agenda-match-entry-node entry) :raw-value)
                   (org-kit:agenda-match-entry-line entry)))
           match-result)))
~~~

`agenda-todo-list` returns an `agenda-result` containing non-DONE TODO
headlines in document order. Its optional `:match` argument filters those
headlines with Org match syntax. `agenda` returns an `agenda-result` with
entries for the inclusive start and end dates. Its call options are
`:warning-days` (14 days before a deadline by default), `:scheduled-delay`
(zero days by default), and `:future-repeats` (default `:all`).
`:scheduled-delay` is a nonnegative integer day count. A scheduled timestamp's
explicit warning amount and unit override `:scheduled-delay`; otherwise that
option supplies the delay in days. This warning amount or option value is the
effective delay used below. The first
scheduled occurrence appears on its scheduled date when its visible date is on
or before that date and the scheduled date is in range. If the scheduled date is
before the reference date, a reminder appears on the reference date once its
visible date has arrived, provided the reference date is in range. For a repeat
row, the visible date must be on or before that occurrence date. On an agenda
entry, `agenda-entry-scheduled-delay-state` returns `:delayed` when the original
scheduled date is before the reference date and the effective delay is positive.
This can mark the reference-date reminder or an eligible repeat row, but not the
original scheduled-date row.

`:future-repeats` can be `:all`, `:next`, or `:none`. For repeating scheduled or
deadline entries, `:all` includes occurrences through the requested inclusive
date range. `:next` includes at most one occurrence strictly after the later of
the original date and reference date; when the original date is already in the
future, it adds no repeat row. `:none` adds no repeat rows. These are call
settings, not document resource limits.

Use `agenda-result-entries` to read rows and `agenda-entry-headline`,
`agenda-entry-kind`, `agenda-entry-date`, `agenda-entry-heading-path`, and the
other `agenda-entry-*` accessors to inspect them. The matcher signature is
`(agenda-match documents match &key time-context)`. It returns matching
headlines in document order as `agenda-match-entry` values, with document,
node, heading path, and source line information. It requires the same explicit
time context as the agenda calls. The `agenda-todo-list` `:match` argument uses
the same Org match syntax and filters non-DONE TODO headlines.

`query-match-now` also requires `:time-context` and evaluates relative dates at
that context's reference instant in its configured time zone. Callers that
used the former implicit system-clock behavior must supply this context.

Agenda rows for a scheduled habit also expose `agenda-entry-habit-consistency`.
It returns one `(:date ... :status ...)` plist for each date in the inclusive
agenda range. Dates use date-only `org-time-value` objects. Status is
`:completed` for a DONE state log on that date or a `CLOSED` date on a done
heading, `:within-schedule` through the due date, and `:late` after that date.
The current `SCHEDULED` timestamp anchors the active occurrence, and its date
is the due date. The latest DONE log uses that due date; earlier due dates are
reconstructed from DONE history according to repeater mode. `.+` places the
next occurrence one repeater interval after each earlier DONE. `+` steps
backward from the current `SCHEDULED` date by one interval for each later DONE.
`++` keeps the cadence and advances past each earlier DONE to the next
occurrence. For historical dates, month repeaters convert at 30.4 days per
month and year repeaters at 365.25 days per year, with the resulting day count
rounded down as in Org's habit history calculation.
Dates after the calculated due date are `:late`, including future dates. Rows for
non-habits return `nil`. A habit must be a
TODO heading with an active scheduled repeater; otherwise `agenda` signals
`org-agenda-error` with a reason such as `:habit-without-repeater`.

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
1,000,000 generated entries, and 10,000 future repeats. A call over multiple
documents uses the lowest configured agenda ceiling among them. These ceilings
can be changed by passing a limits object to `parse-org`, for example
`:limits (org-kit:make-limits :max-agenda-range-days 90)`.

See [Clock](clock.md) for clock aggregation and
[API Stability](../reference/api-stability.md) for the 0.1.0 public API scope.

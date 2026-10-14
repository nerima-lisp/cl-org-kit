# TODO and Agenda

TODO keywords and planning timestamps are stored on parsed headlines. The
public API can set a configured TODO keyword, list unfinished TODO headlines,
build an agenda for a date range, and select headlines with Org match syntax.

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

There is no public helper that advances to the next TODO state. Choose and pass
the state explicitly.

An unknown keyword, invalid target, or conflicting edit signals
`org-edit-error`.

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
explicit warning amount and unit set its visible date; otherwise the visible
date is the scheduled timestamp plus `:scheduled-delay` days. The first
scheduled occurrence appears on its scheduled date when that visible date is
on or before it and the scheduled date is in range. If it is past, it appears
on the reference date once its visible date has arrived, provided that date is
in the inclusive agenda range. `:delayed` is set only when the effective delay
is positive and the original scheduled date is before the reference date. It can
mark the stale reference-date reminder or an eligible future repeat row, but not
the original scheduled-date row.

`:future-repeats` can be `:all`, `:next`, or `:none`. Future repeat rows are
limited to the requested inclusive date range. Repeating scheduled or deadline
entries generate occurrences strictly after the later of the original date and
the time-context reference date. These keyword options are call settings, not
document resource limits.

Use `agenda-result-entries` to read rows and `agenda-entry-headline`,
`agenda-entry-kind`, `agenda-entry-date`, `agenda-entry-heading-path`, and the
other `agenda-entry-*` accessors to inspect them. The matcher signature is
`(agenda-match documents match &key time-context)`. It returns matching
headlines in document order as `agenda-match-entry` values, with document,
node, heading path, and source line information. It requires the same explicit
time context as the agenda calls. `agenda-todo-list`'s `:match` argument uses
the same Org match syntax but only filters its non-DONE TODO headlines.

Invalid agenda options or document lists signal `org-agenda-error`. Invalid
dates or time contexts signal `org-time-error`, malformed match syntax signals
`org-query-error`, and an exceeded range, repeat, or entry ceiling signals
`org-resource-limit-error`. The default limits are 3,660 inclusive days,
1,000,000 generated entries, and 10,000 future repeats. A call over multiple
documents uses the lowest configured agenda ceiling among them. These ceilings
can be changed through the documents' `limits`.

See [Clock](clock.md) for clock aggregation and
[API Stability](../reference/api-stability.md) for the 0.1.0 public API scope.

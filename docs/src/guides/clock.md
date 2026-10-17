# Clock

Clock APIs use explicit time values and an explicit `org-time-context`. The
context provides the time zone and daylight-saving-time policy; these calls do
not consult the system clock.

## Start and stop a clock

`clock-in` and `clock-out` make format-preserving edits. Each returns two
values: an `edit-result` and a timestamp or interval string. Use the updated
document from the result before making another edit.

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

Set these properties with a document line such as
`#+PROPERTY: CLOCK_INTO_DRAWER LOGBOOK`, or in a heading `:PROPERTIES:` drawer.
Heading properties are inherited from ancestors. The `:drawer` argument to
`clock-in` overrides `CLOCK_INTO_DRAWER` for one call. It accepts a drawer name
string or a nonnegative integer threshold; the string `"t"` follows
`LOG_INTO_DRAWER`. A numeric `:drawer` override uses the same threshold rule as
a numeric `CLOCK_INTO_DRAWER` setting.
With no `CLOCK_INTO_DRAWER` setting, `clock-in` writes to a LOGBOOK drawer. A
drawer name selects that drawer; `t` follows `LOG_INTO_DRAWER`, falling back to
LOGBOOK. A numeric `CLOCK_INTO_DRAWER` value is a threshold: `clock-in` uses
LOGBOOK when adding the new clock would bring the heading's clock count to at
least that number; below the threshold it writes the CLOCK line without a drawer.
It refuses to start a second running clock on the same heading. `clock-out`
requires exactly one running clock. Both return edits without changing the input
document.

## Check clock consistency

`check-clock-consistency` returns findings in source order. It reports clock
ranges whose end precedes their start, overlapping ranges, clocks that have no
clock-out, and intervals longer than `:max-duration-minutes` when that optional
threshold is supplied. Touching ranges do not overlap. A running clock is
measured through the reference instant in the required `org-time-context`;
the same instant is used to check its duration and overlap with closed clocks.
Duration-only CLOCK rows are ignored. Malformed CLOCK rows signal
`org-clock-error` with reason `:malformed-clock`.

The number of findings is limited by the document's
`:max-clock-consistency-issues` limit, which defaults to 100,000. If a check
would exceed this ceiling, it signals `org-resource-limit-error`.

Each result is a `clock-consistency-issue`. Its reason and first source line
come from `clock-consistency-issue-reason` and
`clock-consistency-issue-line`. For an overlap,
`clock-consistency-issue-other-line` gives the second line. For a valid interval,
`clock-consistency-issue-duration-minutes` gives its elapsed duration. The
maximum duration is a strict threshold: a clock is too long only when it
exceeds the supplied number of minutes.

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
single-line description after the target heading's planning, property drawers,
and LOGBOOK drawer, before its body, and returns the
`edit-result` and the inserted value. The input document is unchanged; use the
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

## Sum clock time

`sum-clocks` totals a heading and its descendants over the half-open interval
`[FROM, TO)`. The default excludes running clocks. Duration-only CLOCK rows
have no dated interval to clip and are included in the total by default only
when `[FROM, TO)` is nonempty.

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
counts duration-only CLOCK rows, which have no timestamp to clip; they
contribute to the total by default when `[FROM, TO)` is nonempty.
`clock-summary-total-minutes` returns the
sum, and `clock-summary-clipped-interval-count` counts dated intervals with
positive length after clipping. `:include-running-p t` includes running clocks
through the context's reference instant. `:include-unscoped-p nil` excludes
duration-only rows. If your call has a separate `:as-of` cutoff, pass it to
assert that it agrees with the context's reference instant. This catches a
mismatch between the cutoff and the instant used for running clocks. `:as-of`
must resolve to the same instant and does not provide a separate cutoff.

## Generate a clocktable

`clocktable-data` returns a `clocktable-result` for a heading subtree. It
requires a `:range`, a `:time-context`, and accepts optional Org `:match`
syntax and `:max-depth` (default 30). `:max-depth` filters by absolute logical
heading level, not distance from the starting heading. Running clock rows are
excluded. The result contains total minutes and rows in document order. Its row
accessors are `clocktable-row-level`, `clocktable-row-title`,
`clocktable-row-tags`, `clocktable-row-range`, and `clocktable-row-minutes`.
The title is the unformatted heading title. Tags are unique
effective tags from file settings, ancestor headings, and the heading itself.
They are processed in that order, with ancestors from root to parent; a
repeated tag appears once at the position of its most specific occurrence. For
example, an ancestor tagged `:work:focus:` and a heading tagged `:work:` produce
the order `focus`, then `work`. Range is the
supplied `org-time-range`; minutes are clipped over its start-inclusive,
end-exclusive interval and accumulated for that heading's subtree.

`write-clocktable` replaces the body of a parsed `clocktable` dynamic block
with that result and returns an `edit-result`.

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

Invalid clock rows, ranges, or options can signal `org-clock-error` or
`org-time-error`. Clock edits can also signal `org-edit-error`. Clock
operations enforce the document's `:max-node-count`; clocktable writes also
enforce `:max-output-length` and `:max-output-octets`. Their defaults are
4,194,304 nodes, 67,108,864 output characters, and 268,435,456 output octets.
Configure these limits by passing an `org-kit:make-limits` value as `:limits` to
`parse-org`. Exceeding a configured limit signals `org-resource-limit-error`.

# Column view

`column-view-data` returns a table as a list of string-cell rows. The first row
contains column titles. Each later row represents a heading in the selected
scope. The function reads the document without modifying it. Pass an explicit
`:time-context` when an age summary includes timestamps. Duration values can use
age summaries without a time context.

~~~lisp
(let* ((document
         (org-kit:parse-org
          (format nil
                  "#+COLUMNS: %ITEM(Task) %EFFORT{:}~%* Project~%** First~%:PROPERTIES:~%:EFFORT: 1:30~%:END:~%** Second~%:PROPERTIES:~%:EFFORT: 0:45~%:END:~%")))
       (rows (org-kit:column-view-data document)))
  rows)
;; => (("Task" "EFFORT") ("Project" "2:15") ("First" "1:30") ("Second" "0:45"))
~~~

The column format is selected from the nearest `COLUMNS` property on the
selected heading or one of its ancestors, then a file-level `COLUMNS`
property, then `#+COLUMNS`, and finally Org's default format. Pass `:heading`
to restrict the rows to a heading and its subtree.

The format uses `%[WIDTH]PROPERTY[(TITLE)][{SUMMARY-TYPE}]`. The supported
summary types are `+`, `+;%.nf`, `$`, `min`, `max`, `mean`, `X`, `X/`, `X%`,
`:`, `:min`, `:max`, `:mean`, `@min`, `@max`, `@mean`, and `est+`. Summary
values are computed bottom-up from direct children. Existing values on a
parent are ignored when a summary is computed. Summary types are ignored for
special properties such as `ITEM`, `TODO`, `PRIORITY`, `TAGS`, `ALLTAGS`,
`CATEGORY`, `FILE`, `SCHEDULED`, `DEADLINE`, `CLOSED`, `TIMESTAMP`,
`TIMESTAMP_IA`, and `LEVEL`.
Age summaries accept Org timestamps or duration values and format the computed
age as a duration. Timestamp ages use the reference instant and time zone from
`:time-context`.

`#+BEGIN: columnview` is registered as a dynamic block. Use
`update-dynamic-block` to replace its body; only the body is edited, and the
operation returns an `edit-result`. Invalid column definitions and summary
values signal `org-column-view-error`, with a reason such as `:invalid-columns`,
`:invalid-summary-type`, or `:invalid-summary-value`.

; Generated from org-todo with Emacs 31.1 and Org 9.8.10.
; The case timestamp is fixed; log-note text is supplied by the manifest.
((:id "cycle-empty-next" :previous-state nil :new-state "TODO" :source-after "** TODO Empty next
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "cycle-empty-previous" :previous-state nil :new-state "DONE" :source-after "** DONE Empty previous
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "cycle-done-next-clears" :previous-state "DONE" :new-state nil :source-after "** Done next
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "cycle-first-previous-clears" :previous-state "TODO" :new-state nil :source-after "** First previous
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "cycle-second-sequence-next" :previous-state "WAITING" :new-state "REVIEW" :source-after "** REVIEW Cycle next
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "cycle-second-sequence-previous" :previous-state "REVIEW" :new-state "WAITING" :source-after "** WAITING Cycle previous
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "transition-explicit-state" :previous-state "TODO" :new-state "DONE" :source-after "** DONE Explicit state
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "transition-explicit-state-second-sequence" :previous-state "WAITING" :new-state "CANCELLED" :source-after "** CANCELLED Explicit state
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "transition-explicit-clear" :previous-state "TODO" :new-state nil :source-after "** Explicit clear
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "done-log-off" :previous-state "TODO" :new-state "DONE" :source-after "** DONE Done off
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "done-log-time" :previous-state "TODO" :new-state "DONE" :source-after "** DONE Done time
CLOSED: [2025-03-07 Fri 09:00]
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp "[2025-03-07 Fri 09:00]")
 (:id "done-log-note" :previous-state "TODO" :new-state "DONE" :source-after "** DONE Done note
CLOSED: [2025-03-07 Fri 09:00]
:LOGBOOK:
- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\
  oracle closing note
:END:
" :document-after nil :log-events ("- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\" "  oracle closing note") :rescheduled-timestamps nil :closed-timestamp "[2025-03-07 Fri 09:00]")
 (:id "done-note-multiline-blank-line" :previous-state "TODO" :new-state "DONE" :source-after "** DONE Multiline done note
CLOSED: [2025-03-07 Fri 09:00]
:LOGBOOK:
- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\
  first line

  third line
:END:
" :document-after nil :log-events ("- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\" "  first line" "" "  third line") :rescheduled-timestamps nil :closed-timestamp "[2025-03-07 Fri 09:00]")
 (:id "done-note-no-drawer" :previous-state "TODO" :new-state "DONE" :source-after "** DONE Note with no drawer
CLOSED: [2025-03-07 Fri 09:00]
- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\
  oracle note outside drawer
" :document-after nil :log-events ("- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\" "  oracle note outside drawer") :rescheduled-timestamps nil :closed-timestamp "[2025-03-07 Fri 09:00]")
 (:id "done-note-with-existing-indented-body" :previous-state "TODO" :new-state "DONE" :source-after "** DONE Note with existing indented body
CLOSED: [2025-03-07 Fri 09:00]

- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\
  oracle closing note
  Existing body paragraph text
" :document-after nil :log-events ("- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\" "  oracle closing note") :rescheduled-timestamps nil :closed-timestamp "[2025-03-07 Fri 09:00]")
 (:id "done-note-explicit-drawer" :previous-state "TODO" :new-state "DONE" :source-after "** DONE Explicit drawer
CLOSED: [2025-03-07 Fri 09:00]
:CUSTOM:
- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\
  oracle note in custom drawer
:END:
" :document-after nil :log-events ("- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\" "  oracle note in custom drawer") :rescheduled-timestamps nil :closed-timestamp "[2025-03-07 Fri 09:00]")
 (:id "done-note-reuses-existing-drawer" :previous-state "TODO" :new-state "DONE" :source-after "** DONE Reuse existing drawer
CLOSED: [2025-03-07 Fri 09:00]
:LOGBOOK:
- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\
  oracle note in existing drawer
:END:
" :document-after nil :log-events ("- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\" "  oracle note in existing drawer") :rescheduled-timestamps nil :closed-timestamp "[2025-03-07 Fri 09:00]")
 (:id "done-note-with-existing-repeat-property" :previous-state "TODO" :new-state "DONE" :source-after "** DONE Existing repeat property
CLOSED: [2025-03-07 Fri 09:00]
:PROPERTIES:
:LAST_REPEAT: [2025-02-28 Fri 09:00]
:END:
:LOGBOOK:
- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\
  oracle note with existing repeat property
:END:
" :document-after nil :log-events ("- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\" "  oracle note with existing repeat property") :rescheduled-timestamps nil :closed-timestamp "[2025-03-07 Fri 09:00]")
 (:id "done-note-local-drawer-overrides-global" :previous-state "TODO" :new-state "DONE" :source-after "** DONE Local drawer property
CLOSED: [2025-03-07 Fri 09:00]
:PROPERTIES:
:LOG_INTO_DRAWER: LOCAL
:END:
:LOCAL:
- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\
  oracle note in local drawer
:END:
" :document-after nil :log-events ("- CLOSING NOTE [2025-03-07 Fri 09:00] \\\\" "  oracle note in local drawer") :rescheduled-timestamps nil :closed-timestamp "[2025-03-07 Fri 09:00]")
 (:id "existing-body-bullet-with-state-log" :previous-state "TODO" :new-state "NEXT" :source-after "** NEXT Existing body bullet with state log
:LOGBOOK:
- State \"NEXT\"       from \"TODO\"       [2025-03-07 Fri 09:00]
:END:
- Existing body bullet
" :document-after nil :log-events ("- State \"NEXT\"       from \"TODO\"       [2025-03-07 Fri 09:00]") :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "drawerless-state-log-before-indented-body" :previous-state "TODO" :new-state "NEXT" :source-after "** NEXT Drawerless state log before indented body

- State \"NEXT\"       from \"TODO\"       [2025-03-07 Fri 09:00]
  Existing indented paragraph text
" :document-after nil :log-events ("- State \"NEXT\"       from \"TODO\"       [2025-03-07 Fri 09:00]") :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "reopen-without-state-log" :previous-state "DONE" :new-state "TODO" :source-after "** TODO Reopen without logging
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "reopen-with-closed-logging-enabled" :previous-state "DONE" :new-state "TODO" :source-after "** TODO Reopen with closing logging enabled
" :document-after nil :log-events nil :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "marker-enter-time" :previous-state nil :new-state "TODO" :source-after "** TODO Empty enter time
:LOGBOOK:
- State \"TODO\"       from              [2025-03-07 Fri 09:00]
:END:
" :document-after nil :log-events ("- State \"TODO\"       from              [2025-03-07 Fri 09:00]") :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "marker-enter-time-native-default-drawer" :previous-state nil :new-state "TODO" :source-after "** TODO Empty enter time
- State \"TODO\"       from              [2025-03-07 Fri 09:00]
" :document-after nil :log-events ("- State \"TODO\"       from              [2025-03-07 Fri 09:00]") :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "marker-enter-note" :previous-state nil :new-state "NEXT" :source-after "** NEXT Empty enter note
:LOGBOOK:
- State \"NEXT\"       from              [2025-03-07 Fri 09:00] \\\\
  oracle state note
:END:
" :document-after nil :log-events ("- State \"NEXT\"       from              [2025-03-07 Fri 09:00] \\\\" "  oracle state note") :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "marker-exit-time" :previous-state "WAITING" :new-state "REVIEW" :source-after "** REVIEW Leave with exit marker
:LOGBOOK:
- State \"REVIEW\"     from \"WAITING\"    [2025-03-07 Fri 09:00]
:END:
" :document-after nil :log-events ("- State \"REVIEW\"     from \"WAITING\"    [2025-03-07 Fri 09:00]") :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "marker-target-entry-overrides-exit" :previous-state "WAITING" :new-state "DONE" :source-after "** DONE Target entry marker wins
:LOGBOOK:
- State \"DONE\"       from \"WAITING\"    [2025-03-07 Fri 09:00] \\\\
  oracle target-entry note
:END:
" :document-after nil :log-events ("- State \"DONE\"       from \"WAITING\"    [2025-03-07 Fri 09:00] \\\\" "  oracle target-entry note") :rescheduled-timestamps nil :closed-timestamp nil)
 (:id "repeat-plus" :previous-state "TODO" :new-state "TODO" :source-after "** TODO Plus repeater
SCHEDULED: <2025-03-08 Sat +1w>
" :document-after nil :log-events nil :rescheduled-timestamps ("<2025-03-08 Sat +1w>") :closed-timestamp nil)
 (:id "repeat-plus-plus" :previous-state "TODO" :new-state "TODO" :source-after "** TODO Plus plus repeater
SCHEDULED: <2025-03-22 Sat ++1w>
" :document-after nil :log-events nil :rescheduled-timestamps ("<2025-03-22 Sat ++1w>") :closed-timestamp nil)
 (:id "repeat-dot-plus" :previous-state "TODO" :new-state "TODO" :source-after "** TODO Dot plus repeater
SCHEDULED: <2025-03-27 Thu .+1w>
" :document-after nil :log-events nil :rescheduled-timestamps ("<2025-03-27 Thu .+1w>") :closed-timestamp nil)
 (:id "repeat-variable-state" :previous-state "TODO" :new-state "NEXT" :source-after "** NEXT Variable repeat state
SCHEDULED: <2025-03-08 Sat +1w>
" :document-after nil :log-events nil :rescheduled-timestamps ("<2025-03-08 Sat +1w>") :closed-timestamp nil)
 (:id "repeat-local-state-precedence" :previous-state "TODO" :new-state "WAITING" :source-after "** WAITING Local repeat state
:PROPERTIES:
:REPEAT_TO_STATE: WAITING
:END:
SCHEDULED: <2025-03-08 Sat +1w>
" :document-after "#+TODO: TODO NEXT WAITING | DONE
* Repeat cases
** TODO Plus repeater
SCHEDULED: <2025-03-01 Sat +1w>
** TODO Plus plus repeater
SCHEDULED: <2025-03-01 Sat ++1w>
** TODO Dot plus repeater
SCHEDULED: <2025-03-01 Sat .+1w>
** TODO Variable repeat state
SCHEDULED: <2025-03-01 Sat +1w>
** WAITING Local repeat state
:PROPERTIES:
:REPEAT_TO_STATE: WAITING
:END:
SCHEDULED: <2025-03-08 Sat +1w>
** TODO Repeat without log or clock
SCHEDULED: <2025-03-01 Sat +1w>
** TODO Repeat with log
SCHEDULED: <2025-03-01 Sat +1w>
** TODO Repeat with existing LAST_REPEAT and log
:PROPERTIES:
:LAST_REPEAT: [2025-02-28 Fri 09:00]
:END:
SCHEDULED: <2025-03-01 Sat +1w>
** TODO Plus plus with note log and LAST_REPEAT
:PROPERTIES:
:LAST_REPEAT: [2025-02-28 Fri 09:00]
:END:
SCHEDULED: <2025-03-01 Sat ++1w>
** TODO Repeat with clock
CLOCK: [2025-03-19 Wed 08:00]--[2025-03-19 Wed 08:15] =>  0:15
SCHEDULED: <2025-03-01 Sat +1w>
** TODO Remove nonrepeat schedule
SCHEDULED: <2025-03-01 Sat> DEADLINE: <2025-03-01 Sat +1w>
Plain timestamp: <2025-03-01 Sat +1w>
** TODO Remove schedule on completion
SCHEDULED: <2025-03-25 Tue>
** TODO Remove schedule before body repeater
SCHEDULED: <2025-03-01 Sat>
Repeating body timestamp: <2025-03-01 Sat +1w>
" :log-events nil :rescheduled-timestamps ("<2025-03-08 Sat +1w>") :closed-timestamp nil)
 (:id "repeat-no-last-repeat-without-log-or-clock" :previous-state "TODO" :new-state "TODO" :source-after "** TODO Repeat without log or clock
SCHEDULED: <2025-03-08 Sat +1w>
" :document-after nil :log-events nil :rescheduled-timestamps ("<2025-03-08 Sat +1w>") :closed-timestamp nil)
 (:id "repeat-last-repeat-with-log" :previous-state "TODO" :new-state "TODO" :source-after "** TODO Repeat with log
SCHEDULED: <2025-03-08 Sat +1w>
:PROPERTIES:
:LAST_REPEAT: [2025-03-20 Thu 09:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2025-03-20 Thu 09:00]
:END:
" :document-after nil :log-events ("- State \"DONE\"       from \"TODO\"       [2025-03-20 Thu 09:00]") :rescheduled-timestamps ("<2025-03-08 Sat +1w>") :closed-timestamp nil)
 (:id "repeat-overwrites-existing-last-repeat-with-log" :previous-state "TODO" :new-state "TODO" :source-after "** TODO Repeat with existing LAST_REPEAT and log
:PROPERTIES:
:LAST_REPEAT: [2025-03-20 Thu 09:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2025-03-20 Thu 09:00]
:END:
SCHEDULED: <2025-03-08 Sat +1w>
" :document-after nil :log-events ("- State \"DONE\"       from \"TODO\"       [2025-03-20 Thu 09:00]") :rescheduled-timestamps ("<2025-03-08 Sat +1w>") :closed-timestamp nil)
 (:id "repeat-plus-plus-note-log-last-repeat-and-done-log" :previous-state "TODO" :new-state "TODO" :source-after "** TODO Plus plus with note log and LAST_REPEAT
:PROPERTIES:
:LAST_REPEAT: [2025-03-20 Thu 09:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2025-03-20 Thu 09:00] \\\\
  oracle repeat note
:END:
SCHEDULED: <2025-03-22 Sat ++1w>
" :document-after nil :log-events ("- State \"DONE\"       from \"TODO\"       [2025-03-20 Thu 09:00] \\\\" "  oracle repeat note") :rescheduled-timestamps ("<2025-03-22 Sat ++1w>") :closed-timestamp nil)
 (:id "repeat-last-repeat-with-clock" :previous-state "TODO" :new-state "TODO" :source-after "** TODO Repeat with clock
:PROPERTIES:
:LAST_REPEAT: [2025-03-20 Thu 09:00]
:END:
CLOCK: [2025-03-19 Wed 08:00]--[2025-03-19 Wed 08:15] =>  0:15
SCHEDULED: <2025-03-08 Sat +1w>
" :document-after nil :log-events nil :rescheduled-timestamps ("<2025-03-08 Sat +1w>") :closed-timestamp nil)
 (:id "repeat-removes-nonrepeat-scheduled" :previous-state "TODO" :new-state "TODO" :source-after "** TODO Remove nonrepeat schedule
DEADLINE: <2025-03-08 Sat +1w>
Plain timestamp: <2025-03-08 Sat +1w>
" :document-after nil :log-events nil :rescheduled-timestamps ("<2025-03-08 Sat +1w>" "<2025-03-08 Sat +1w>") :closed-timestamp nil)
 (:id "repeat-removes-scheduled-before-body-repeater" :previous-state "TODO" :new-state "TODO" :source-after "** TODO Remove schedule before body repeater
Repeating body timestamp: <2025-03-08 Sat +1w>
" :document-after nil :log-events nil :rescheduled-timestamps ("<2025-03-08 Sat +1w>") :closed-timestamp nil)
 (:id "cookie-child-completes" :previous-state "TODO" :new-state "DONE" :source-after "** DONE Child
" :document-after "#+TODO: TODO | DONE
* Parent [1/1]
** DONE Child
" :log-events nil :rescheduled-timestamps nil :closed-timestamp nil))

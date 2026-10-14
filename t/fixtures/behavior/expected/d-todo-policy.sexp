((:id "built-in-logbook-policy"
  :previous-state nil
  :new-state "TODO"
  :source-after "** TODO Default drawer
:LOGBOOK:
- State \"TODO\"       from              [2025-03-07 Fri 09:00]
:END:
"
  :document-after nil
  :log-events ("- State \"TODO\"       from              [2025-03-07 Fri 09:00]")
  :rescheduled-timestamps nil
  :closed-timestamp nil)
 (:id "completion-removes-nonrepeat-scheduled-policy"
  :previous-state "TODO"
  :new-state "DONE"
  :source-after "* DONE Remove nonrepeat schedule
"
  :document-after nil
  :log-events nil
  :rescheduled-timestamps nil
  :closed-timestamp nil))

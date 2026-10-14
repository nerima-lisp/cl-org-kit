(:cases
 ((:id "cycle-empty-next"
   :source "d-todo-cycle.org"
   :heading-path ("Cycle cases" "Empty next")
   :action :cycle :direction :next
   :at "2025-03-07T09:00:00")
  (:id "cycle-empty-previous"
   :source "d-todo-cycle.org"
   :heading-path ("Cycle cases" "Empty previous")
   :action :cycle :direction :previous
   :at "2025-03-07T09:00:00")
  (:id "cycle-done-next-clears"
   :source "d-todo-cycle.org"
   :heading-path ("Cycle cases" "Done next")
   :action :cycle :direction :next
   :at "2025-03-07T09:00:00")
  (:id "cycle-first-previous-clears"
   :source "d-todo-cycle.org"
   :heading-path ("Cycle cases" "First previous")
   :action :cycle :direction :previous
   :at "2025-03-07T09:00:00")
  (:id "cycle-second-sequence-next"
   :source "d-todo-sequence.org"
   :heading-path ("Second sequence cases" "Cycle next")
   :action :cycle :direction :next :sequence-id "seq-todo-1"
   :at "2025-03-07T09:00:00")
  (:id "cycle-second-sequence-previous"
   :source "d-todo-sequence.org"
   :heading-path ("Second sequence cases" "Cycle previous")
   :action :cycle :direction :previous :sequence-id "seq-todo-1"
   :at "2025-03-07T09:00:00")
  (:id "transition-explicit-state"
   :source "d-todo-cycle.org"
   :heading-path ("Cycle cases" "Explicit state")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00")
  (:id "transition-explicit-state-second-sequence"
   :source "d-todo-sequence.org"
   :heading-path ("Second sequence cases" "Explicit state")
   :action :transition :state "CANCELLED"
   :at "2025-03-07T09:00:00")
  (:id "transition-explicit-clear"
   :source "d-todo-cycle.org"
   :heading-path ("Cycle cases" "Explicit clear")
   :action :transition :state nil
   :at "2025-03-07T09:00:00")
  (:id "done-log-off"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Done off")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00"
   :log-done nil :log-into-drawer nil)
  (:id "done-log-time"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Done time")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00"
   :log-done time :log-into-drawer nil)
  (:id "done-log-note"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Done note")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00"
   :log-done note :log-into-drawer "LOGBOOK"
   :note "oracle closing note")
  (:id "done-note-multiline-blank-line"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Multiline done note")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00"
   :log-done note :log-into-drawer "LOGBOOK"
   :note "first line

third line")
  (:id "done-note-no-drawer"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Note with no drawer")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00"
   :log-done note :log-into-drawer nil
   :note "oracle note outside drawer")
  (:id "done-note-with-existing-indented-body"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Note with existing indented body")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00"
   :log-done note :log-into-drawer nil
   :note "oracle closing note")
  (:id "done-note-explicit-drawer"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Explicit drawer")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00"
   :log-done note :log-into-drawer "CUSTOM"
   :note "oracle note in custom drawer")
  (:id "done-note-reuses-existing-drawer"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Reuse existing drawer")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00"
   :log-done note :log-into-drawer "LOGBOOK"
   :note "oracle note in existing drawer")
  (:id "done-note-with-existing-repeat-property"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Existing repeat property")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00"
   :log-done note :log-into-drawer "LOGBOOK"
   :note "oracle note with existing repeat property")
  (:id "done-note-local-drawer-overrides-global"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Local drawer property")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00"
   :log-done note :log-into-drawer "GLOBAL"
   :note "oracle note in local drawer")
  (:id "existing-body-bullet-with-state-log"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Existing body bullet with state log")
   :action :transition :state "NEXT"
   :at "2025-03-07T09:00:00"
   :log-into-drawer "LOGBOOK")
  (:id "drawerless-state-log-before-indented-body"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Drawerless state log before indented body")
   :action :transition :state "NEXT"
   :at "2025-03-07T09:00:00"
   :log-into-drawer nil)
  (:id "reopen-without-state-log"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Reopen without logging")
   :action :transition :state "TODO"
   :at "2025-03-07T09:00:00"
   :log-done nil :log-into-drawer nil)
  (:id "reopen-with-closed-logging-enabled"
   :source "d-todo-logging.org"
   :heading-path ("Logging cases" "Reopen with closing logging enabled")
   :action :transition :state "TODO"
   :at "2025-03-07T09:00:00"
   :log-done time :log-into-drawer nil)
  (:id "marker-enter-time"
   :source "d-todo-markers.org"
   :heading-path ("Marker cases" "Empty enter time")
   :action :cycle :direction :next
   :at "2025-03-07T09:00:00"
   :log-into-drawer "LOGBOOK")
  (:id "marker-enter-time-native-default-drawer"
   :source "d-todo-markers.org"
   :heading-path ("Marker cases" "Empty enter time")
   :action :cycle :direction :next
   :at "2025-03-07T09:00:00"
   :log-into-drawer nil)
  (:id "marker-enter-note"
   :source "d-todo-markers.org"
   :heading-path ("Marker cases" "Empty enter note")
   :action :transition :state "NEXT"
   :at "2025-03-07T09:00:00"
   :log-into-drawer "LOGBOOK"
   :note "oracle state note")
  (:id "marker-exit-time"
   :source "d-todo-markers.org"
   :heading-path ("Marker cases" "Leave with exit marker")
   :action :transition :state "REVIEW"
   :at "2025-03-07T09:00:00"
   :log-into-drawer "LOGBOOK")
  (:id "marker-target-entry-overrides-exit"
   :source "d-todo-markers.org"
   :heading-path ("Marker cases" "Target entry marker wins")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00"
   :log-into-drawer "LOGBOOK"
   :note "oracle target-entry note")
  (:id "repeat-plus"
   :source "d-todo-repeat.org"
   :heading-path ("Repeat cases" "Plus repeater")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :log-repeat nil :log-into-drawer nil)
  (:id "repeat-plus-plus"
   :source "d-todo-repeat.org"
   :heading-path ("Repeat cases" "Plus plus repeater")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :log-repeat nil :log-into-drawer nil)
  (:id "repeat-dot-plus"
   :source "d-todo-repeat.org"
   :heading-path ("Repeat cases" "Dot plus repeater")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :log-repeat nil :log-into-drawer nil)
  (:id "repeat-variable-state"
   :source "d-todo-repeat.org"
   :heading-path ("Repeat cases" "Variable repeat state")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :repeat-to-state "NEXT" :log-repeat nil :log-into-drawer nil)
  (:id "repeat-local-state-precedence"
   :source "d-todo-repeat.org"
   :heading-path ("Repeat cases" "Local repeat state")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :repeat-to-state "NEXT" :log-repeat nil :log-into-drawer nil
   :capture-document t)
  (:id "repeat-no-last-repeat-without-log-or-clock"
   :source "d-todo-repeat.org"
   :heading-path ("Repeat cases" "Repeat without log or clock")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :log-repeat nil :log-into-drawer nil)
  (:id "repeat-last-repeat-with-log"
   :source "d-todo-repeat.org"
   :heading-path ("Repeat cases" "Repeat with log")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :log-repeat time :log-into-drawer "LOGBOOK")
  (:id "repeat-overwrites-existing-last-repeat-with-log"
   :source "d-todo-repeat.org"
   :heading-path ("Repeat cases" "Repeat with existing LAST_REPEAT and log")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :log-repeat time :log-into-drawer "LOGBOOK")
  (:id "repeat-plus-plus-note-log-last-repeat-and-done-log"
   :source "d-todo-repeat.org"
   :heading-path ("Repeat cases" "Plus plus with note log and LAST_REPEAT")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :log-done time :log-repeat note :log-into-drawer "LOGBOOK"
   :note "oracle repeat note")
  (:id "repeat-last-repeat-with-clock"
   :source "d-todo-repeat.org"
   :heading-path ("Repeat cases" "Repeat with clock")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :log-repeat nil :log-into-drawer nil)
  (:id "repeat-removes-nonrepeat-scheduled"
   :source "d-todo-repeat.org"
   :heading-path ("Repeat cases" "Remove nonrepeat schedule")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :log-repeat nil :log-into-drawer nil)
  (:id "repeat-removes-scheduled-before-body-repeater"
   :source "d-todo-repeat.org"
   :heading-path ("Repeat cases" "Remove schedule before body repeater")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :log-repeat nil :log-into-drawer nil)
  (:id "cookie-child-completes"
   :source "d-todo-cookie.org"
   :heading-path ("Parent [0/1]" "Child")
   :action :transition :state "DONE"
   :at "2025-03-07T09:00:00"
   :capture-document t)))

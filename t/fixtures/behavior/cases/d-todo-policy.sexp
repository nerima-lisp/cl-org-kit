(:cases
 ((:id "built-in-logbook-policy"
   :source "policy/d-todo-logbook.org"
   :heading-path ("Policy" "Default drawer")
   :action :cycle :direction :next
   :at "2025-03-07T09:00:00")
  (:id "completion-removes-nonrepeat-scheduled-policy"
   :source "policy/d-todo-scheduled.org"
   :heading-path ("Remove nonrepeat schedule")
   :action :transition :state "DONE"
   :at "2025-03-20T09:00:00"
   :log-done nil :log-into-drawer nil)))

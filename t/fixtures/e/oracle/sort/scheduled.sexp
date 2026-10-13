; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "scheduled" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :scheduled
       :org-type :headline :sort-character 115 :input
       "* Parent
** Later
SCHEDULED: <2024-03-01 Fri>
** Early
SCHEDULED: <2024-02-01 Thu>
** Tie first
SCHEDULED: <2024-02-01 Thu>
** Unscheduled
" :expected
       "* Parent
** Early
SCHEDULED: <2024-02-01 Thu>
** Tie first
SCHEDULED: <2024-02-01 Thu>
** Later
SCHEDULED: <2024-03-01 Fri>
** Unscheduled
" :expected-order
       ("Early" "Tie first"
				    "Later"
				    "Unscheduled")
       :expected-key-values
       ("<2024-02-01 Thu>" "<2024-02-01 Thu>"
				    "<2024-03-01 Fri>" nil))

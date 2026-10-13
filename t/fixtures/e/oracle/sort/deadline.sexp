; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "deadline" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :deadline
       :org-type :headline :sort-character 100 :input
       "* Parent
** Later
DEADLINE: <2024-03-01 Fri>
** Early
DEADLINE: <2024-02-01 Thu>
** Tie first
DEADLINE: <2024-02-01 Thu>
** No deadline
" :expected
       "* Parent
** Early
DEADLINE: <2024-02-01 Thu>
** Tie first
DEADLINE: <2024-02-01 Thu>
** Later
DEADLINE: <2024-03-01 Fri>
** No deadline
" :expected-order
       ("Early" "Tie first"
				    "Later"
				    "No deadline")
       :expected-key-values
       ("<2024-02-01 Thu>" "<2024-02-01 Thu>"
				    "<2024-03-01 Fri>" nil))

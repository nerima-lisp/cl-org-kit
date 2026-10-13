; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "time" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :time
       :org-type :headline :sort-character 116 :input
       "* Parent
** Later <2024-03-01 Fri>
** Early <2024-02-01 Thu>
** Tie first <2024-02-01 Thu>
** No timestamp
" :expected
       "* Parent
** Early <2024-02-01 Thu>
** Tie first <2024-02-01 Thu>
** Later <2024-03-01 Fri>
** No timestamp
" :expected-order
       ("Early <2024-02-01 Thu>" "Tie first <2024-02-01 Thu>"
				    "Later <2024-03-01 Fri>"
				    "No timestamp")
       :expected-key-values
       ("<2024-02-01 Thu>" "<2024-02-01 Thu>"
				    "<2024-03-01 Fri>" nil))

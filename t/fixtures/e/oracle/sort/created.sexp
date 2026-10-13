; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "created" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :created
       :org-type :headline :sort-character 114 :property-name
       "CREATED" :input "* Parent
** Later
:PROPERTIES:
:CREATED: [2024-03-01 Fri]
:END:
** Early
:PROPERTIES:
:CREATED: [2024-02-01 Thu]
:END:
** Tie first
:PROPERTIES:
:CREATED: [2024-02-01 Thu]
:END:
"
       :expected "* Parent
** Early
:PROPERTIES:
:CREATED: [2024-02-01 Thu]
:END:
** Tie first
:PROPERTIES:
:CREATED: [2024-02-01 Thu]
:END:
** Later
:PROPERTIES:
:CREATED: [2024-03-01 Fri]
:END:
" :expected-order
       ("Early" "Tie first"
				    "Later")
       :expected-key-values
       ("[2024-02-01 Thu]" "[2024-02-01 Thu]"
				    "[2024-03-01 Fri]"))

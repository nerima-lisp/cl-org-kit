; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "created-missing-last" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :created
       :property-name "CREATED" :divergence
       :org-created-missing-sorts-first :input
       "* Parent
** Later
:PROPERTIES:
:CREATED: [2024-03-01 Fri]
:END:
** Missing
** Early
:PROPERTIES:
:CREATED: [2024-02-01 Thu]
:END:
** Tie first
:PROPERTIES:
:CREATED: [2024-02-01 Thu]
:END:
" :expected-order
       ("Early" "Tie first"
				    "Later"
				    "Missing")
       :expected-key-values
       ("[2024-02-01 Thu]" "[2024-02-01 Thu]"
				    "[2024-03-01 Fri]" nil)
       :oracle-expected-order
       ("Missing" "Early"
				     "Tie first"
				     "Later")
       :oracle-key-values
       (nil "[2024-02-01 Thu]" "[2024-02-01 Thu]"
	    "[2024-03-01 Fri]"))

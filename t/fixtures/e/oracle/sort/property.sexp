; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "property" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :property
       :org-type :headline :sort-character 114 :property-name
       "RANK" :input "* Parent
** Zulu
:PROPERTIES:
:RANK: z
:END:
** Alpha first
:PROPERTIES:
:RANK: a
:END:
** Alpha second
:PROPERTIES:
:RANK: a
:END:
"
       :expected "* Parent
** Alpha first
:PROPERTIES:
:RANK: a
:END:
** Alpha second
:PROPERTIES:
:RANK: a
:END:
** Zulu
:PROPERTIES:
:RANK: z
:END:
" :expected-order
       ("Alpha first" "Alpha second"
				    "Zulu")
       :expected-key-values
       ("a" "a"
				    "z"))

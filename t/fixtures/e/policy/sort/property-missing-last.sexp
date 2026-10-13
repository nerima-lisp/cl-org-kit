; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "property-missing-last" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :property
       :property-name "RANK" :divergence
       :org-property-missing-sorts-first :input
       "* Parent
** Zulu
:PROPERTIES:
:RANK: z
:END:
** Missing property
** Alpha
:PROPERTIES:
:RANK: a
:END:
" :expected-order
       ("Alpha" "Zulu"
				    "Missing property")
       :expected-key-values
       ("a" "z" nil)
       :oracle-expected-order
       ("Missing property" "Alpha"
				     "Zulu")
       :oracle-key-values
       (nil "a" "z"))

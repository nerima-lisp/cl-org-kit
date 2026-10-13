; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "numeric" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :numeric
       :org-type :headline :sort-character 110 :input
       "* Parent
** 10
** 2
** 2.0
** 1
" :expected
       "* Parent
** 1
** 2
** 2.0
** 10
" :expected-order
       ("1" "2"
				    "2.0"
				    "10")
       :expected-key-values
       ("1" "2"
				    "2.0"
				    "10"))

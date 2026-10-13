; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "alpha" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :alpha
       :org-type :headline :sort-character 97 :input
       "* Parent
** beta
** Alpha
** alpha
** alpha
** Gamma
" :expected
       "* Parent
** Alpha
** alpha
** alpha
** beta
** Gamma
" :expected-order
       ("Alpha" "alpha"
				    "alpha"
				    "beta"
				    "Gamma")
       :expected-key-values
       ("Alpha" "alpha"
				     "alpha"
				     "beta"
				     "Gamma"))

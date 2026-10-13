; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "todo" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :todo
       :org-type :headline :sort-character 111 :input
       "#+TODO: TODO WAIT | DONE
* Parent
** WAIT Waiting
** DONE Finished
** TODO Active
" :expected
       "#+TODO: TODO WAIT | DONE
* Parent
** TODO Active
** WAIT Waiting
** DONE Finished
" :expected-order
       ("TODO Active" "WAIT Waiting"
				    "DONE Finished")
       :expected-key-values
       ("TODO" "WAIT"
				    "DONE"))

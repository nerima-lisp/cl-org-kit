; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "todo-missing-last" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :todo
       :property-name nil :divergence :org-todo-missing-is-not-last
       :input "#+TODO: TODO WAIT | DONE
* Parent
** DONE Finished
** No TODO state
** WAIT Waiting
** TODO Active
" :expected-order
       ("TODO Active" "WAIT Waiting"
				    "DONE Finished"
				    "No TODO state")
       :expected-key-values
       ("TODO" "WAIT"
				    "DONE" nil)
       :oracle-expected-order
       ("TODO Active" "WAIT Waiting"
				     "No TODO state"
				     "DONE Finished")
       :oracle-key-values
       ("TODO" "WAIT" nil
				     "DONE"))

; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "priority-directive" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :priority
       :org-type :headline :sort-character 112 :priority-highest
       "B" :priority-default
       "C" :priority-lowest
       "D" :input "#+PRIORITIES: B D C
* Parent
** [#C] Explicit default
** [#D] Lowest
** Default priority
** [#B] Highest
"
       :expected "#+PRIORITIES: B D C
* Parent
** [#B] Highest
** [#C] Explicit default
** Default priority
** [#D] Lowest
" :expected-order
       ("[#B] Highest" "[#C] Explicit default"
				    "Default priority"
				    "[#D] Lowest")
       :expected-key-values (2000 1000 1000 0))

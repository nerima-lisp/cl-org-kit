; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "priority" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :priority
       :org-type :headline :sort-character 112 :priority-highest
       "A" :priority-default
       "B" :priority-lowest
       "C" :input "* Parent
** [#B] Explicit B
** Default priority
** [#A] High
** [#C] Low
** [#B] Second B
"
       :expected "* Parent
** [#A] High
** [#B] Explicit B
** Default priority
** [#B] Second B
** [#C] Low
" :expected-order
       ("[#A] High" "[#B] Explicit B"
				    "Default priority"
				    "[#B] Second B"
				    "[#C] Low")
       :expected-key-values (2000 1000 1000 1000 0))

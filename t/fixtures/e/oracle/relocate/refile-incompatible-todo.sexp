; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "refile-incompatible-todo" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :refile :mode :move
       :target-heading "Target" :source-input
       "#+TODO: WAIT | CLOSED
* WAIT Source
source body
" :target-input
       "#+TODO: TODO | DONE
* Target
target body
" :source-expected
       "#+TODO: WAIT | CLOSED
" :target-expected
       "#+TODO: TODO | DONE
* Target
target body
** WAIT Source
source body
")

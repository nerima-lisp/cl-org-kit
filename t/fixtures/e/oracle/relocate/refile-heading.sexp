; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "refile-heading" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :refile :mode :move
       :target-heading "Target" :source-input
       "* Parent
** Source
source body
" :target-input
       "* Target
target body
" :source-expected
       "* Parent
" :target-expected
       "* Target
target body
** Source
source body
")

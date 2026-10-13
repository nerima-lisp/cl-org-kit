; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "alpha-code-point-case-sensitive" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :sort :key :alpha
       :property-name nil :divergence :org-alpha-is-case-insensitive
       :input "* Parent
** a
** A
" :expected-order
       ("A" "a")
       :expected-key-values
       ("A" "a")
       :oracle-expected-order
       ("a" "A")
       :oracle-key-values
       ("a" "A"))

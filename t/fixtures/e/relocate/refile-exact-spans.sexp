(:name "refile-exact-spans"
 :source-heading "Source"
 :target-heading "Target"
 :mode :move
 :source-input "* Parent
** Source
source body
* Tail
tail body
"
 :target-input "* Target
target body
"
 :source-expected "* Parent
* Tail
tail body
"
 :target-expected "* Target
target body
** Source
source body
")

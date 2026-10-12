`branch-gap-source.lisp` is wholly excluded as a `defconstant`, while its
coverage row also reports an uncovered branch. The former checker accepted the
row because its only uncovered line was excluded. The fixed checker rejects it
because coverage summaries do not identify branch locations.

To compare the checker at the current base with the fixed checker:

```sh
git show f5c3860:scripts/check-coverage.pl > /tmp/check-coverage-before.pl
sh scripts/coverage-reproductions/check-branch-gap.sh /tmp/check-coverage-before.pl 0
sh scripts/coverage-reproductions/check-branch-gap.sh scripts/check-coverage.pl 1
```

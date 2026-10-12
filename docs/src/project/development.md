# Development

`flake.nix` is one `cl-nix-forge` `mkPackageFlake` call. Run every command
from the repository root.

```sh
nix flake check          # every gate below
nix develop              # SBCL, Perl and paredit with dependencies on the registry
nix run .#test           # the test suite
nix build .#docs         # the documentation site
nix fmt                  # format the Nix files
```

Inside `nix develop`, iterate with:

```sh
sbcl --script run-tests.lisp
```

## Gates

`nix flake check` fails when any of these fails:

- `checks.default`: `run-tests.lisp` compiles both systems and fails on any
  non-style warning, and on any style warning raised from `src/`. It then runs
  the cl-weave suite. An empty suite fails. The scripts under `scripts/` are
  compiled by the same warning check.
- `checks.formatting`: treefmt over the Nix files.
- `checks.docs`: `mkdocs build --strict`.
- `checks.paredit-lint`: the paredit-cli lint over the Lisp sources.
- `checks.coverage`: `scripts/run-coverage.lisp` runs the suite under
  SB-COVER and `scripts/check-coverage.pl` requires 100% line and branch
  coverage for every file in `src/`. The only exempt forms are the categories
  in `scripts/coverage-exclusions.sexp`, each with a reason and a reproduction
  under `scripts/coverage-reproductions/`.

`t/public-api-test.lisp` pins the exported symbol list, requires a docstring
on every export, and pins the package name with no nicknames. Adding or
removing an export means editing that list in the same change.

Run the coverage gate by hand with the output kept out of the tree:

```sh
COVERAGE_OUTPUT=/tmp/org-cov/data.coverage \
COVERAGE_REPORT_DIRECTORY=/tmp/org-cov/report/ \
COVERAGE_SUMMARY=/tmp/org-cov/per-file.tsv \
COVERAGE_ASDF_CACHE=/tmp/org-cov/cache/ \
  sbcl --script scripts/run-coverage.lisp
```

## Mutation testing

`scripts/run-mutation.lisp` mutates the functions listed in its `*targets*`
with cl-weave and runs only the specs matching each target's name filter. It
exits non-zero when a mutant survives or errors, when a target selects no
spec, or when a target yields no mutant. It is run by hand, not by
`nix flake check`:

```sh
sbcl --dynamic-space-size 4096 --non-interactive --load scripts/run-mutation.lisp
```

## Dependencies

Sibling libraries are pinned to release tags as `flake = false` inputs.
`cl-regex-kit` stays on the tag aitools pins, so embedding this library there
adds no second version to its closure.

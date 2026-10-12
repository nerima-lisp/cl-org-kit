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
  the cl-weave suite. An empty suite fails.
- `checks.formatting`: treefmt over the Nix files.
- `checks.docs`: `mkdocs build --strict`.
- `checks.paredit-lint`: the paredit-cli lint over the Lisp sources.

`t/public-api-test.lisp` pins the exported symbol list, requires a docstring
on every export, and pins the package name with no nicknames. Adding or
removing an export means editing that list in the same change.

## Dependencies

Sibling libraries are pinned to release tags as `flake = false` inputs.
`cl-regex-kit` stays on the tag aitools pins, so embedding this library there
adds no second version to its closure.

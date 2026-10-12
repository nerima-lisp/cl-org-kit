# Getting Started

`cl-org-kit` supports SBCL only. Its runtime dependencies are
`cl-parser-kit`, `cl-regex-kit`, `cl-codec-kit`, and `cl-date-kit`; the test
system also uses `cl-weave`.

## Nix

```nix
inputs.cl-org-kit = {
  url = "github:nerima-lisp/cl-org-kit/v0.1.0";
  flake = false;
};
```

Pin a release tag rather than following the default branch.

## ASDF

Put this checkout and its dependencies on `CL_SOURCE_REGISTRY`, then:

```lisp
(asdf:load-system "cl-org-kit")
```

The package is `ORG-KIT`. It has no nicknames.

# cl-org-kit

`cl-org-kit` is a Common Lisp library for reading, writing, querying, and
editing Org mode documents, following the
[Org Syntax](https://orgmode.org/worg/org-syntax.html) specification (v2).
It is under construction.

The supported implementation is SBCL. Runtime dependencies are
`cl-parser-kit`, `cl-regex-kit`, `cl-codec-kit`, and `cl-date-kit`.

## Install

Load the `cl-org-kit` ASDF system from a directory on `CL_SOURCE_REGISTRY`.
See [`docs/src/getting-started.md`](docs/src/getting-started.md).

```lisp
(asdf:load-system "cl-org-kit")
```

## Development

See [`docs/src/project/development.md`](docs/src/project/development.md).

## License

MIT. See [`LICENSE`](LICENSE).
Unicode 17.0.0 width data notices: [`THIRD-PARTY-NOTICES`](THIRD-PARTY-NOTICES).

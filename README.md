# cl-org-kit

`cl-org-kit` is a Common Lisp library for reading, writing, querying, and
editing Org mode documents. It follows the
[Org Syntax](https://orgmode.org/worg/org-syntax.html) specification (v2) and
targets SBCL.

## Features

- Parse Org documents into a source-spanned node tree. The lossless writer
  preserves unchanged source text; the canonical writer emits normalized Org.
- Query headings with structured selectors, path-and-filter strings, or Org
  tag/property match syntax.
- Make format-preserving edits to TODO keywords, headings, properties,
  planning data, subtrees, checkboxes, clocks, and Org pipe tables.
- Build TODO lists and date-range agendas from one or more documents.
- Start and stop clocks, summarize clock time, and create clocktable data.
- Format, edit, and stably sort Org pipe tables.
- Export to HTML and GitHub Flavored Markdown (GFM).

The 0.1.0 public API does not provide ASCII export or a table-formula
recalculation function. See the [feature guides](docs/src/guides/todo-agenda.md)
and [API stability scope](docs/src/reference/api-stability.md) for supported
calls, return values, conditions, and limits.

Runtime dependencies are `cl-parser-kit`, `cl-regex-kit`, `cl-codec-kit`, and
`cl-date-kit`.

## Install

Load the `cl-org-kit` ASDF system from a directory on `CL_SOURCE_REGISTRY`.
See [Getting Started](docs/src/getting-started.md).

~~~lisp
(asdf:load-system "cl-org-kit")
~~~

## Development

See [Development](docs/src/project/development.md).

## License

MIT. See [LICENSE](LICENSE).
Unicode 17.0.0 width data notices: [THIRD-PARTY-NOTICES](THIRD-PARTY-NOTICES).

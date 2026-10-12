# cl-org-kit

`cl-org-kit` is a Common Lisp library for Org mode documents. It targets the
[Org Syntax](https://orgmode.org/worg/org-syntax.html) specification (v2):
every element and object becomes a node that keeps its source span, so an
unmodified document is written back byte for byte.

The library is under construction. The planned surface is a parser, a lossless
and a canonical writer, a query engine accepting three selector forms
(structured lists, path-and-filter strings, and Org's tag/property match
syntax), and a format-preserving editor.

Start with [Getting Started](getting-started.md). Contributor workflow is in
[Development](project/development.md).

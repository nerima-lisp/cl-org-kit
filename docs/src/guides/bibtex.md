# BibTeX

`bibtex-import` appends BibTeX entries to an Org document as top-level
headings. It returns an `edit-result`; read the updated document from
`edit-result-updated-document`. Import leaves the input document unchanged.
`bibtex-export` serializes one heading as a BibTeX entry.

From the repository root, start the project development shell with
`nix develop --command sbcl`, then evaluate this block in the REPL. It returns
two strings: updated Org text beginning with `* Analytical Engine`, followed
by BibTeX output beginning with `@article{key42,`.

```lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(labels ((first-headline (document)
           (let ((found nil))
             (org-kit:map-nodes
              (lambda (node)
                (when (and (null found)
                           (eq (org-kit:node-type node) :headline))
                  (setf found node)))
              (org-kit:document-root document))
             found)))
  (let* ((document (org-kit:parse-org ""))
         (result
           (org-kit:bibtex-import
            document
            "@Article{key42, author={Ada   Lovelace}, title={Analytical Engine}, journal={Notes}, year={1843}}"))
         (updated (org-kit:edit-result-updated-document result))
         (heading (first-headline updated)))
    (list (org-kit:document-text updated)
          (org-kit:bibtex-export updated heading))))
```

Import preserves entry order and appends all headings at the original document
end in one insertion span in the returned edit result. Its start and end
offsets are both the original document length.
Each heading stores the lowercase entry type in `BTYPE`, the BibTeX key in
`CUSTOM_ID`, and the title in both the heading and `TITLE` property. Imported
field names become uppercase properties. A matching outer brace or quote pair
is removed when present. Runs of spaces, tabs, line feeds, and carriage returns
in a value become one space, including at the beginning and end. Vertical tabs
and form feeds are preserved.

Pass `:prefix` as a string without a colon or line break, for example
`"bib-"`. It applies to `TITLE`, `BTYPE`, and field properties; `CUSTOM_ID`
remains unprefixed. Pass the same prefix to `bibtex-export` to read those
properties.

The supported entry types are `article`, `book`, `booklet`, `conference`,
`inbook`, `incollection`, `inproceedings`, `manual`, `mastersthesis`, `misc`,
`phdthesis`, `proceedings`, `techreport`, and `unpublished`. Import accepts
entries delimited by braces or parentheses, with comma-separated braced,
quoted, or bare field values. Bare macro names and `#` concatenations remain
field text; the library does not evaluate them. Import signals
`org-bibtex-error` for empty input, text outside an entry, missing types, keys,
or field names, malformed delimiters or field syntax, duplicate fields, and
unclosed values or entries. Unsupported types signal the same condition with
reason `:unknown-type`.

Export follows the field lists and order in Org 9.8.10's `org-bibtex-types`
variable in the top-level source file `ol-bibtex.el` in the
[Org 9.8.10 source archive](https://elpa.gnu.org/packages/org-9.8.10.tar).
Extra imported properties remain on the heading but are omitted from export.
For example, `article` exports present fields in `author`, `title`, `journal`,
`year`, `volume`, `number`, `pages`, `month`, `note`, `doi` order.
When the title property is absent, export uses the heading title. An empty
title property stays empty. Values are wrapped in braces without escaping, and
the output ends with a newline.

Conversion does not check whether a type's required fields are present. It
copies the supported fields that are present; missing fields do not cause a
completeness error. Org's separate interactive `org-bibtex-check` and
`org-bibtex-fleshout` functions are not run by this library. An imported entry
needs a `title` field because Org's default headline formatter uses it directly
and does not fall back to the citation key. An empty title is allowed; if the
field is absent, import signals `org-bibtex-error` with reason `:missing-title`.
Import and export do not read or write files and do not invoke Emacs.

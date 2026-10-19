# BibTeX

`bibtex-import` appends BibTeX entries to an Org document as top-level
headings. It returns an `edit-result`; read the updated document from
`edit-result-updated-document`. `bibtex-export` serializes one heading as a
BibTeX entry.

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

Import preserves entry order and appends all imported headings in one edit.
Each heading stores the lowercase entry type in `BTYPE`, the BibTeX key in
`CUSTOM_ID`, and the title in both the heading and `TITLE` property. Imported
field names become uppercase properties. One outer brace or quote pair is
removed from each value, and whitespace runs collapse to one space.

Pass `:prefix` to add a property prefix. It applies to `TITLE`, `BTYPE`, and
field properties; `CUSTOM_ID` remains unprefixed. Pass the same prefix to
`bibtex-export` to read those properties.

The supported entry types are `article`, `book`, `booklet`, `conference`,
`inbook`, `incollection`, `inproceedings`, `manual`, `mastersthesis`, `misc`,
`phdthesis`, `proceedings`, `techreport`, and `unpublished`. Import accepts
entries delimited by braces or parentheses, with comma-separated braced,
quoted, or bare field values. It does not resolve BibTeX string macros or
concatenation expressions. Unknown types, missing keys, and malformed entries
signal `org-bibtex-error`.

Export follows Org's default required and optional field order for each type.
It includes only fields listed for that type; extra imported properties remain
on the heading but are omitted from export. The title property supplies the
exported title, with the heading title as a fallback. Values are wrapped in
braces without escaping, and the output ends with a newline.

Org prompts interactively when required fields are missing. This library has no
interactive prompt, so import and export signal `org-bibtex-error` with reason
`:missing-required-fields`. A `misc` entry can omit its title; in that case its
key supplies the imported heading title. These operations do not read or write
files and do not invoke Emacs.

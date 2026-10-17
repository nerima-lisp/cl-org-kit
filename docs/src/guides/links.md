# Links

Org links are parsed as `:link` nodes with a type and path. The parser
recognizes `id:` and `attachment:` as link types. `resolve-link` resolves IDs,
custom IDs, fuzzy links, radio links, and file links with `::` search options.
It leaves `attachment:` links unresolved because there is no attachment
resolver yet. `#+LINK` definitions support `%s` and `%h` substitutions.

## Resolve a link

`resolve-link` accepts a parsed link node and either one document or a list of
documents. Its `link-resolution-status` is `:resolved`, `:unresolved`, or
`:ambiguous`. Check the status before using `link-resolution-target`; inspect
`link-resolution-candidates` when a link has more than one possible target.
ID lookup can span every document passed to the call.

~~~lisp
(let* ((document
         (org-kit:parse-org
          (format nil
                  "* Overview~%:PROPERTIES:~%:CUSTOM_ID: overview~%:END:~%[[#overview]]~%")))
       (link (find :link
                   (let ((nodes nil))
                     (org-kit:map-nodes
                      (lambda (node)
                        (when (eq (org-kit:node-type node) :link)
                          (push node nodes)))
                      (org-kit:document-root document))
                     (nreverse nodes))
                   :key #'org-kit:node-type))
       (resolution (org-kit:resolve-link link document)))
  (list (org-kit:link-resolution-status resolution)
        (org-kit:link-resolution-target resolution)))
~~~

For `file:` links, pass a caller-owned resolver. In this example, `link` is a
parsed link node, `document` is its parsed parent document, and
`source-by-path` is a caller-owned hash table mapping paths to Org source text:

~~~lisp
(org-kit:resolve-link
 link document
 :file-resolver (lambda (path parent-source)
                  (declare (ignore parent-source))
                  (gethash path source-by-path)))
~~~

The callback receives the link path and the `document-source-name` of the
document containing the link, or `nil` if it has no source name. It must return
Org source text, a parsed document, or `nil` when the file is unavailable.
`resolve-link` does not open paths: the caller decides how to resolve relative
paths, restrict access to a root, and handle symlinks before returning content.

The `::` suffix selects a search target inside the resolved document. For
example, `file:manual.org::*Overview` matches a heading, `file:manual.org::#setup`
matches a `CUSTOM_ID`, `file:manual.org::<<intro>>` matches a target,
`file:manual.org::/needle/` searches text with a regular expression, and
`file:manual.org::needle` searches plain text. `file:::*Overview` searches the
current document and does not call the file resolver.

## Link abbreviations

`#+LINK` definitions expand the suffix after the link type with `%s` verbatim
or `%h` as UTF-8 bytes percent-encoded with uppercase hex digits; ASCII letters,
digits, `-`, `.`, `_`, and `~` remain unescaped. A definition that uses
`%(fn)` needs an Emacs Lisp evaluator, so this feature signals
`org-link-resolution-error` with reason `:unsupported-link-function` and a
context excerpt limited to 80 input characters with control characters
escaped. Inspect `org-link-resolution-error-reason` and
`org-link-resolution-error-context` for details. No function is evaluated
during link resolution.

HTML export calls `resolve-link` to map internal links to exported anchors. Call
`resolve-link` directly when you need the resolution status or candidate list.

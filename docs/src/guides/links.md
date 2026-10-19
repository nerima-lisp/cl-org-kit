# Links

Org links are parsed as `:link` nodes with a type and path. The parser
recognizes `id:` and `attachment:` as link types. `resolve-link` resolves IDs,
custom IDs, fuzzy links, radio links, and file links with `::` search options.
It leaves `attachment:` links unresolved because there is no attachment
resolver yet. Radio links match radio targets written as `<<<text>>>`. Fuzzy
links search explicit targets, named elements, and then headings in that order.
They ignore case and normalize whitespace; heading
search also ignores statistics cookies. A `[[*Heading]]` link searches headings
only, and `CUSTOM_ID` values are case-sensitive.

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
 :file-resolver (lambda (path parent-source-name)
                  (declare (ignore parent-source-name))
                  (gethash path source-by-path)))
~~~

The callback receives the link path and the containing document's
`document-source-name` as its second argument, or `nil` if it has no source
name. It must return
Org source text, a parsed document, or `nil` when the file is unavailable.
`resolve-link` does not open paths: the caller decides how to resolve relative
paths, restrict access to a root, and handle symlinks before returning content.

The `::` suffix selects a search target inside the resolved document. For
example, `file:manual.org::*Overview` matches a heading, `file:manual.org::#setup`
matches a `CUSTOM_ID`, `file:manual.org::<<intro>>` matches a target,
`file:manual.org::/needle/` searches text with a regular expression, and
`file:manual.org::needle` searches plain text. `file:::*Overview` searches the
current document and does not call the file resolver. The default maximum
regular-expression length is 4096 characters and can be set with
`:max-search-regex-length` in `make-limits`. `:max-search-work` defaults to
64 Mi characters and caps the candidate text submitted to regular-expression
matching for each searched document within one link-resolution index. HTML
export reuses one index for the whole export, so its regex links share that
budget. Each direct `resolve-link` call creates a fresh index and budget. HTML
export leaves a link unresolved if this limit is exceeded.

## Link abbreviations

`#+LINK` definitions expand the suffix after the link type by replacing the
first `%s` verbatim, or the first `%h` as UTF-8 bytes percent-encoded with
uppercase hex digits; ASCII letters, digits, `-`, `.`, `_`, and `~` remain
unescaped. `%s` takes precedence when both forms occur. The expanded link is
checked against the document's `:max-input-length` limit. A definition that uses
`%(fn)` needs an Emacs Lisp evaluator, so this feature signals
`org-link-resolution-error` with reason `:unsupported-link-function` and a
context excerpt limited to 80 input characters with control characters
escaped. Inspect `org-link-resolution-error-reason` and
`org-link-resolution-error-context` for details. No function is evaluated
during link resolution. Invalid regular expressions also signal
`org-link-resolution-error`; regular expressions longer than the configured
limit signal `org-resource-limit-error`.

HTML export leaves links unresolved when resolution reports an unsupported
abbreviation or an invalid search expression. Call `resolve-link` directly
when you need the resolution status, candidate list, or diagnostic condition.

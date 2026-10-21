# Org Lint

`org-lint` checks an already parsed document and returns
`org-lint-diagnostic` values sorted by position. It uses the existing syntax
tree and source spans; it does not parse the document again. Each diagnostic
contains the Org checker name, a 1-based character offset matching Emacs buffer
positions, and the exact Org message.

~~~lisp
(let* ((document (org-kit:parse-org "Text [fn:missing]."))
       (findings (org-kit:org-lint
                  document
                  :checkers '("undefined-footnote-reference"))))
  (mapcar (lambda (finding)
            (list (org-kit:org-lint-diagnostic-checker finding)
                  (org-kit:org-lint-diagnostic-position finding)
                  (org-kit:org-lint-diagnostic-message finding)))
          findings))
;; => (("undefined-footnote-reference" 6
;;      "Missing definition for footnote [missing]"))
~~~

By default, `org-lint` selects every implemented checker. Pass `:checkers` with
a list of strings containing Org checker names to limit the run; an explicitly
empty list selects none. An unknown name signals `org-kit-error`. `:context` is
a plist for checks that need caller-owned runtime or filesystem state. A
context-dependent checker runs only when all its required keys are present;
otherwise it is skipped without a warning. The library does not infer the
caller's Babel registry, export backends, ID database, macro configuration, or
filesystem view.

Supply complete caller snapshots for registries and indexes so that known
entries are not mistaken for unknown ones. The
`:babel-header-arguments` context key is a flat list for
`deprecated-header-syntax`; `:babel-header-specifications` supplies the
common and language-specific header registry data used by the two
`wrong-header-*` checks. `:babel-effective-exports` supplies the resolved
source-block values used by `named-result`. The table lists all 61 unique
active checker names in the Org 9.8.10 registry.

| Org checker | Input | Check |
|---|---|---|
| `misplaced-heading` | Parsed document | Heading syntax that appears after text on the same line. |
| `duplicate-custom-id` | Parsed document | Repeated `CUSTOM_ID` values. |
| `duplicate-name` | Parsed document | Repeated `NAME` values. |
| `duplicate-target` | Parsed document | Repeated Org targets. |
| `duplicate-footnote-definition` | Parsed document | Repeated footnote labels. |
| `orphaned-affiliated-keywords` | Parsed document | Affiliated keywords with no element. |
| `combining-keywords-with-affiliated` | Parsed document | Independent keywords immediately before affiliated keywords. |
| `obsolete-affiliated-keywords` | Parsed document | Deprecated affiliated keyword names. |
| `deprecated-export-blocks` | Parsed document | Deprecated backend-specific export block syntax. |
| `deprecated-header-syntax` | Caller context | Deprecated Babel header properties; requires `:babel-header-arguments`. |
| `missing-language-in-src-block` | Parsed document | Source blocks without a language. |
| `suspicious-language-in-src-block` | Caller context | Source block languages absent from `:source-languages`. |
| `missing-backend-in-export-block` | Parsed document | Export blocks without a backend. |
| `invalid-babel-call-block` | Parsed document | Invalid Babel call syntax or end headers. |
| `wrong-header-argument` | Caller context | Header argument names absent from `:babel-header-specifications`. |
| `wrong-header-value` | Caller context | Header values or combinations rejected by `:babel-header-specifications`. |
| `named-result` | Caller context | Named `#+RESULTS` elements whose parent source blocks do not export results, using `:babel-effective-exports`. |
| `empty-header-argument` | Parsed document | Valueless tokens in source-block, attached `#+HEADER`/`#+HEADERS` lines, inline-source, inline Babel-call, and `#+CALL` headers. |
| `deprecated-category-setup` | Parsed document | Misuse of the `CATEGORY` keyword. |
| `invalid-coderef-link` | Parsed document | Code reference links with unknown destinations. |
| `invalid-custom-id-link` | Parsed document | Custom ID links with unknown destinations. |
| `invalid-fuzzy-link` | Parsed document | Fuzzy links with unknown document locations. |
| `invalid-id-link` | Caller context | ID links absent from `:known-ids`. |
| `trailing-bracket-after-link` | Parsed document | A possibly confusing `]` after a link. |
| `unclosed-brackets-in-link-description` | Parsed document | An unmatched `[` inside a link description. |
| `link-to-local-file` | Caller context | File links checked with `:file-exists-p`; attachment links use optional `:attachment-file-exists-p`. |
| `non-existent-setupfile-parameter` | Caller context | Setup files rejected by `:file-exists-p`. |
| `wrong-include-link-parameter` | Caller context | Include paths rejected by `:file-exists-p`; optional search validation uses `:include-target-p`. |
| `obsolete-include-markup` | Parsed document | Deprecated markup in `INCLUDE` keywords. |
| `unknown-options-item` | Caller context | `OPTIONS` items absent from `:export-options`. |
| `misspelled-export-option` | Caller context | Potentially misspelled export property names, using `:export-options`. |
| `invalid-macro-argument-and-template` | Caller context | Macro arguments and templates, using `:macro-definitions` and `:macro-templates`. |
| `special-property-in-properties-drawer` | Parsed document | Special properties placed in a properties drawer. |
| `obsolete-properties-drawer` | Parsed document | Incorrect properties drawer contents or location. |
| `invalid-effort-property` | Parsed document | Invalid `EFFORT` duration values. |
| `invalid-id-property` | Parsed document | `ID` properties containing `::`. |
| `undefined-footnote-reference` | Parsed document | Footnote references without definitions. |
| `unreferenced-footnote-definition` | Parsed document | Footnote definitions with no references. |
| `extraneous-element-in-footnote-section` | Parsed document | Non-footnote content in a footnote section. |
| `invalid-keyword-syntax` | Parsed document | Lines that may be misspelled keywords. |
| `invalid-image-alignment` | Parsed document | Unsupported image alignment attributes. |
| `invalid-block` | Parsed document | Invalid block closers or incomplete blocks. |
| `mismatched-planning-repeaters` | Parsed document | Different repeaters in `SCHEDULED` and `DEADLINE`. |
| `misplaced-planning-info` | Parsed document | Planning information in a misplaced line. |
| `incomplete-drawer` | Parsed document | Likely incomplete or misleading drawers. |
| `indented-diary-sexp` | Parsed document | Indented diary sexps. |
| `quote-section` | Parsed document | Deprecated headlines and inline tasks named `QUOTE ...` or `COMMENT QUOTE ...`. |
| `file-application` | Parsed document | Deprecated `file+application` links. |
| `percent-encoding-link-escape` | Parsed document | Links using obsolete percent-encoding escapes. |
| `spurious-colons` | Parsed document | Tags containing an extra colon. |
| `non-existent-bibliography` | Caller context | Bibliography paths rejected by `:file-exists-p`. |
| `missing-print-bibliography` | Parsed document | Citations with no `PRINT_BIBLIOGRAPHY` keyword. |
| `invalid-cite-export-declaration` | Caller context | Cite export processors absent from `:cite-export-processors`. |
| `incomplete-citation` | Parsed document | Incomplete citation markup. |
| `item-number` | Parsed document | Inconsistent ordered list counters. |
| `priority` | Parsed document | Invalid, malformed, or out-of-bounds priorities. |
| `LaTeX-$` | Parsed document | Dollar signs that may be mistaken for LaTeX fragment boundaries. |
| `beamer-frame` | Caller context | Frame environment text, using `:beamer-frame-environment`. |
| `timestamp-syntax` | Parsed document | Potentially malformed timestamps. |
| `clock-syntax` | Parsed document | Potentially malformed clock lines. |
| `planning-inactive` | Parsed document | Inactive timestamps in `SCHEDULED` or `DEADLINE`. |

An argument is empty when Org's header parser returns no value for its token.
This includes bare `:results` and single tokens such as `:results=` or
`:results=foo`, which Org treats as argument names. `:results= output` has a
value because `output` is separated by whitespace. The diagnostic message uses
Org's parsed argument name.

The `empty-header-argument` diagnostic uses the start position of its associated
block or inline element, following Org's checker.

Org's `named-result` check reports a named `#+RESULTS` element when its parent
source block's effective `:exports` value is neither `results` nor `both`. The
caller supplies that final value for each source block through
`:babel-effective-exports`. The local parser reads `#+RESULTS[hash]` as the
keyword name `RESULTS[hash]`, not as `RESULTS` with a hash property, so the
checker cannot associate that result form with its source block and skips it.
Use `#+RESULTS: block` without a `[hash]` suffix for a result the local parser
can associate:

~~~org
#+NAME: block
#+BEGIN_SRC emacs-lisp :exports code
(+ 1 2)
#+END_SRC
#+NAME: result
#+RESULTS: block
: 3
~~~

## Caller context

Pass only the material relevant to the checks you select.

| Context key | Used by |
|---|---|
| `:babel-header-arguments` | `deprecated-header-syntax` |
| `:babel-header-specifications` | `wrong-header-argument`, `wrong-header-value` |
| `:babel-effective-exports` | `named-result` |
| `:source-languages` | `suspicious-language-in-src-block` |
| `:known-ids` | `invalid-id-link` |
| `:file-exists-p` | `file:` links in `link-to-local-file`, `non-existent-setupfile-parameter`, `wrong-include-link-parameter`, `non-existent-bibliography` |
| `:attachment-file-exists-p` (optional) | `link-to-local-file` for `attachment:` links |
| `:include-target-p` (optional) | Search-part validation in `wrong-include-link-parameter` |
| `:export-options` | `unknown-options-item`, `misspelled-export-option` |
| `:macro-definitions` and `:macro-templates` | `invalid-macro-argument-and-template` |
| `:cite-export-processors` | `invalid-cite-export-declaration` |
| `:beamer-frame-environment` | `beamer-frame` |

`:babel-header-specifications` is a plist with `:complete t`, `:common`, and
`:languages` entries. `:common` is an alist from header names without a leading
colon to allowed values; `:languages` is an alist from language names to
language-specific header-name alists. A header's allowed-value entry is `nil`
or `:any` to accept any value. Otherwise it is a list of groups containing
allowed strings; a group may contain `:any` to accept other values. Explicit
values in the same group are mutually exclusive, while values from separate
groups can coexist.
For `wrong-header-argument`, common names are allowed with names from the
source block's language; if the block has no language, names from all listed
languages are allowed. For `wrong-header-value`, a language-specific value
definition overrides the common definition for the same header. The common
definition applies when the source block's language has no definition for that
header.
Both checks skip when this context is missing, `:complete` is not `t`, or the
schema is malformed. The caller's `:complete t` marker asserts that the
snapshot covers its active registries. The checker validates the schema and
marker but cannot compare the snapshot with the caller's actual registries.
This shape example describes a deliberately reduced registry, not the stock
Org registry:

~~~lisp
(list :babel-header-specifications
      '(:complete t
        :common (("exports" ("code" "results" "both" "none"))
                 ("cache" ("no" "yes")))
        :languages (("emacs-lisp" ("lexical" . :any)))))
~~~

It is complete only for a caller whose active registry contains exactly these
entries. A stock Org setup has more common entries in
`org-babel-common-header-args-w-values` and entries for its loaded Babel
languages, such as `org-babel-header-args:emacs-lisp`. Include all of those
active entries before setting `:complete` to `t` in a real snapshot.

`:babel-effective-exports` is an alist from each source block's zero-based
`node-begin` offset to its final effective `:exports` string. Traverse the
document tree to collect its `:src-block` nodes, then derive each value from
`org-kit:resolve-babel-parameters` when its defaults match the caller's:

~~~lisp
(labels ((source-blocks (node)
           (append (when (eq (org-kit:node-type node) :src-block)
                     (list node))
                   (loop for child in (org-kit:node-children node)
                         append (source-blocks child)))))
  (list :babel-effective-exports
        (loop for source-block in
                (source-blocks (org-kit:document-root document))
              for parameters =
                (org-kit:resolve-babel-parameters document source-block)
              for exports =
                (cdr (assoc "EXPORTS" parameters :test #'string=))
              when (stringp exports)
                collect (cons (org-kit:node-begin source-block) exports))))
~~~

The caller must supply the final value after applying all relevant Org defaults
and header arguments. Resolve each block using the caller's active defaults,
document properties, affiliated headers, and block headers.
`resolve-babel-parameters` applies the library's defaults and document
properties. For active Emacs Org defaults, parse the same buffer text with
`org-kit:parse-org` and collect its `:src-block` nodes. Collect the buffer's Org
`src-block` elements with
`(org-element-map (org-element-parse-buffer) 'src-block #'identity)`. Match each
element to the local node whose `org-kit:node-begin` equals one less than the
element's `:begin` position. This converts Emacs's one-based buffer position to
the library's zero-based offset and includes affiliated lines in both starts.
Use `:begin`, not `:post-affiliated`, because the latter points to the
`#+BEGIN_SRC` line after affiliated lines. The start offset also distinguishes
repeated blocks with identical text. For each matched element, call
`(org-babel-get-src-block-info t element)` to resolve parameters with the active
Babel defaults without evaluating parameter values. The returned list's third
item is the parameter alist; read `:exports` from it and include only string
values. If no node matches an element, omit that entry. The checker skips a
result when its source block or effective value is unavailable.

`:file-exists-p` is a one-argument predicate. For `file:` links and `INCLUDE`,
the checker passes the file path before the first `::` search suffix. For a
quoted `INCLUDE` path, it first removes the surrounding double quotes. For
`SETUPFILE` and `BIBLIOGRAPHY`, it trims surrounding whitespace and removes one
matching pair of surrounding double quotes, then passes the remaining value
as-is. This lets the caller resolve relative paths against the right base
directory without making the library assume a process working directory.
`link-to-local-file` still requires `:file-exists-p` to run. Attachment links
use the optional `:attachment-file-exists-p` callback with the path and link
node; when absent, attachment links are skipped. The callback may return a
second string value to use in the diagnostic. `:include-target-p` receives the
include file path and search part. To check search text against included-file
contents, the predicate can close over contents loaded by the caller. If it is
absent, only the path checks run.

List and alist context values such as `:source-languages`, `:known-ids`,
`:cite-export-processors`, `:export-options`, and macro data are complete
snapshots of caller-known entries. Empty lists are valid when the corresponding
registry or index is empty. `:babel-header-arguments` is a list of argument-name
strings; a leading colon is optional. Backend-specific property names use
`backend:KEY`, such as `html:HTML_HEAD`. Document `#+MACRO` declarations take
precedence over caller data. When no document declaration matches, the checker
looks in `:macro-definitions` and then `:macro-templates`; if both contain the
same name, `:macro-definitions` wins. Both are alists from string macro names to
string templates. `:beamer-frame-environment` is the configured environment
name as a string.

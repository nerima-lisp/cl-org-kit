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
empty list selects none. An unknown name signals `org-kit-error`. Deferred
checkers are accepted when selected but produce no findings.
`:context` is a plist for checks that need caller-owned runtime or filesystem
state. A context-dependent checker runs only when all its required keys are
present; otherwise it is skipped without a warning. The library does not infer
the caller's Babel registry, export backends, ID database, macro configuration,
or filesystem view.

The Input column identifies what a checker reads: the parsed document, caller
context, Babel registry data, or result/export data. Parsed-document checks
need no additional context; caller-context checks need the values described
below. Checks that require the common or language-specific Babel registries or
result/export associations are deferred. The `:babel-header-arguments` context
key is a flat list for `deprecated-header-syntax`; it does not supply those
registries. The table lists all 61 unique checker names from Org 9.8.10.

| Org checker | Input | Status | Check |
|---|---|---|---|
| `misplaced-heading` | Parsed document | Implemented | Heading syntax that appears after text on the same line. |
| `duplicate-custom-id` | Parsed document | Implemented | Repeated `CUSTOM_ID` values. |
| `duplicate-name` | Parsed document | Implemented | Repeated `NAME` values. |
| `duplicate-target` | Parsed document | Implemented | Repeated Org targets. |
| `duplicate-footnote-definition` | Parsed document | Implemented | Repeated footnote labels. |
| `orphaned-affiliated-keywords` | Parsed document | Implemented | Affiliated keywords with no element. |
| `combining-keywords-with-affiliated` | Parsed document | Implemented | Independent keywords immediately before affiliated keywords. |
| `obsolete-affiliated-keywords` | Parsed document | Implemented | Deprecated affiliated keyword names. |
| `deprecated-export-blocks` | Parsed document | Implemented | Deprecated backend-specific export block syntax. |
| `deprecated-header-syntax` | Caller context | Implemented | Deprecated Babel header properties; requires `:babel-header-arguments`. |
| `missing-language-in-src-block` | Parsed document | Implemented | Source blocks without a language. |
| `suspicious-language-in-src-block` | Caller context | Implemented | Source block languages absent from `:source-languages`. |
| `missing-backend-in-export-block` | Parsed document | Implemented | Export blocks without a backend. |
| `invalid-babel-call-block` | Parsed document | Implemented | Invalid Babel call syntax or end headers. |
| `wrong-header-argument` | Babel registry data | Deferred | Header argument names absent from the common or language-specific Babel registry. |
| `wrong-header-value` | Babel registry data | Deferred | Header values or combinations rejected by the common or language-specific Babel registry. |
| `named-result` | Result/export data | Deferred | Named `#+RESULTS` elements whose parent source blocks do not export results. |
| `empty-header-argument` | Parsed document | Implemented | Valueless tokens in source-block, attached `#+HEADER`/`#+HEADERS` lines, inline-source, inline Babel-call, and `CALL` headers. |
| `deprecated-category-setup` | Parsed document | Implemented | Misuse of the `CATEGORY` keyword. |
| `invalid-coderef-link` | Parsed document | Implemented | Code reference links with unknown destinations. |
| `invalid-custom-id-link` | Parsed document | Implemented | Custom ID links with unknown destinations. |
| `invalid-fuzzy-link` | Parsed document | Implemented | Fuzzy links with unknown document locations. |
| `invalid-id-link` | Caller context | Implemented | ID links absent from `:known-ids`. |
| `trailing-bracket-after-link` | Parsed document | Implemented | A possibly confusing `]` after a link. |
| `unclosed-brackets-in-link-description` | Parsed document | Implemented | An unmatched `[` inside a link description. |
| `link-to-local-file` | Caller context | Implemented | Local links or images rejected by `:file-exists-p`. |
| `non-existent-setupfile-parameter` | Caller context | Implemented | Setup files rejected by `:file-exists-p`. |
| `wrong-include-link-parameter` | Caller context | Implemented | Include paths rejected by `:file-exists-p`; optional search validation uses `:include-target-p`. |
| `obsolete-include-markup` | Parsed document | Implemented | Deprecated markup in `INCLUDE` keywords. |
| `unknown-options-item` | Caller context | Implemented | `OPTIONS` items absent from `:export-options`. |
| `misspelled-export-option` | Caller context | Implemented | Potentially misspelled export property names, using `:export-options`. |
| `invalid-macro-argument-and-template` | Caller context | Implemented | Macro arguments and templates, using `:macro-definitions` and `:macro-templates`. |
| `special-property-in-properties-drawer` | Parsed document | Implemented | Special properties placed in a properties drawer. |
| `obsolete-properties-drawer` | Parsed document | Implemented | Incorrect properties drawer contents or location. |
| `invalid-effort-property` | Parsed document | Implemented | Invalid `EFFORT` duration values. |
| `invalid-id-property` | Parsed document | Implemented | `ID` properties containing `::`. |
| `undefined-footnote-reference` | Parsed document | Implemented | Footnote references without definitions. |
| `unreferenced-footnote-definition` | Parsed document | Implemented | Footnote definitions with no references. |
| `extraneous-element-in-footnote-section` | Parsed document | Implemented | Non-footnote content in a footnote section. |
| `invalid-keyword-syntax` | Parsed document | Implemented | Lines that may be misspelled keywords. |
| `invalid-image-alignment` | Parsed document | Implemented | Unsupported image alignment attributes. |
| `invalid-block` | Parsed document | Implemented | Invalid block closers or incomplete blocks. |
| `mismatched-planning-repeaters` | Parsed document | Implemented | Different repeaters in `SCHEDULED` and `DEADLINE`. |
| `misplaced-planning-info` | Parsed document | Implemented | Planning information in a misplaced line. |
| `incomplete-drawer` | Parsed document | Implemented | Likely incomplete or misleading drawers. |
| `indented-diary-sexp` | Parsed document | Implemented | Indented diary sexps. |
| `quote-section` | Parsed document | Implemented | Deprecated headlines and inline tasks named `QUOTE ...` or `COMMENT QUOTE ...`. |
| `file-application` | Parsed document | Implemented | Deprecated `file+application` links. |
| `percent-encoding-link-escape` | Parsed document | Implemented | Links using obsolete percent-encoding escapes. |
| `spurious-colons` | Parsed document | Implemented | Tags containing an extra colon. |
| `non-existent-bibliography` | Caller context | Implemented | Bibliography paths rejected by `:file-exists-p`. |
| `missing-print-bibliography` | Parsed document | Implemented | Citations with no `PRINT_BIBLIOGRAPHY` keyword. |
| `invalid-cite-export-declaration` | Caller context | Implemented | Cite export processors absent from `:cite-export-processors`. |
| `incomplete-citation` | Parsed document | Implemented | Incomplete citation markup. |
| `item-number` | Parsed document | Implemented | Inconsistent ordered list counters. |
| `priority` | Parsed document | Implemented | Invalid, malformed, or out-of-bounds priorities. |
| `LaTeX-$` | Parsed document | Implemented | Dollar signs that may be mistaken for LaTeX fragment boundaries. |
| `beamer-frame` | Caller context | Implemented | Frame environment text, using `:beamer-frame-environment`. |
| `timestamp-syntax` | Parsed document | Implemented | Potentially malformed timestamps. |
| `clock-syntax` | Parsed document | Implemented | Potentially malformed clock lines. |
| `planning-inactive` | Parsed document | Implemented | Inactive timestamps in `SCHEDULED` or `DEADLINE`. |

An argument is empty when Org's header parser returns no value for its token.
This includes bare `:results` and single tokens such as `:results=` or
`:results=foo`, which Org treats as argument names. `:results= output` has a
value because `output` is separated by whitespace. The diagnostic message uses
Org's parsed argument name.

The `empty-header-argument` diagnostic uses the start position of its associated
block or inline element, following Org's checker.

Of the 61 checker names, 58 are implemented and selected by default. Of those,
46 need only the parsed document and 12 run only when their required context
keys are present. Three are deferred and not dispatched.

Org's `named-result` check reports a named `#+RESULTS` element when its parent
source block does not export results
through `:exports results` or `:exports both`. Its effective `:exports` may
come from Org's global or language-specific Babel defaults. Also, `parse-org`
reads `#+RESULTS[hash]` as the keyword name `RESULTS[hash]`, not as `RESULTS`
with a hash property, so it cannot associate that result with its source block.
Without the effective defaults and that association, the checker could report
a result that Org considers exportable.

## Caller context

Pass only the material relevant to the checks you select. Empty collections are
valid supplied data when they represent an empty caller registry or index.

| Context key | Used by |
|---|---|
| `:babel-header-arguments` | `deprecated-header-syntax` |
| `:source-languages` | `suspicious-language-in-src-block` |
| `:known-ids` | `invalid-id-link` |
| `:file-exists-p` | `link-to-local-file`, `non-existent-setupfile-parameter`, `wrong-include-link-parameter`, `non-existent-bibliography` |
| `:include-target-p` (optional) | Search-part validation in `wrong-include-link-parameter` |
| `:export-options` | `unknown-options-item`, `misspelled-export-option` |
| `:macro-definitions` and `:macro-templates` | `invalid-macro-argument-and-template` |
| `:cite-export-processors` | `invalid-cite-export-declaration` |
| `:beamer-frame-environment` | `beamer-frame` |

`:file-exists-p` is a one-argument predicate. For local links and `INCLUDE`,
the checker passes the file path before the first `::` search suffix. For a
quoted `INCLUDE` path, it first removes the surrounding double quotes. For
`SETUPFILE` and `BIBLIOGRAPHY`, it trims surrounding whitespace and removes one
matching pair of surrounding double quotes, then passes the remaining value
as-is. This lets the caller resolve relative paths against the right base
directory, including `attachment:` links, without making the library assume a
process working directory. `:include-target-p` receives the include file path
and search part. To check search text against included-file contents, the
predicate can close over contents loaded by the caller. If it is absent, only
the path checks run. `:source-languages`, `:known-ids`, and
`:cite-export-processors` are lists of strings. `:babel-header-arguments` is a
list of argument-name strings; a leading colon is optional. `:export-options`
is a list of option names. Backend-specific property names use `backend:KEY`,
such as `html:HTML_HEAD`. Document `#+MACRO` declarations take precedence over
caller data. When no document declaration matches, the checker looks in
`:macro-definitions` and then `:macro-templates`; both are alists from string
macro names to string templates. `:beamer-frame-environment` is the configured
environment name as a string.

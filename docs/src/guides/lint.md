# Org Lint

`org-lint` checks an already parsed document and returns an ordered list of
`org-lint-diagnostic` values. It uses the existing syntax tree and source spans;
it does not parse the document again. Each diagnostic contains the Org checker
name, its 1-based character position, and the exact Org message. Positions match
Emacs buffer positions, where the first character is position 1.

~~~lisp
(let* ((document (org-kit:parse-org "Text [fn:missing]."))
       (findings (org-kit:org-lint document)))
  (mapcar (lambda (finding)
            (list (org-kit:org-lint-diagnostic-checker finding)
                  (org-kit:org-lint-diagnostic-position finding)
                  (org-kit:org-lint-diagnostic-message finding)))
          findings))
~~~

`org-lint` runs every dispatchable checker by default. Pass `:checkers` with a
list of Org checker names to limit the run; an explicitly empty list selects
none. An unknown name signals `org-kit-error`. A known dependency-wait checker
is accepted but produces no findings while it remains deferred. `:context` is
a plist for checks that need caller-owned runtime or filesystem state. A
context-dependent checker runs only when all its required keys are present.
The library does not infer the caller's Babel registry, export backends, ID
database, macro configuration, or filesystem view.

"Document only" checks require only parsed document data. "Caller material"
checks also need the named context values described below. The table covers all
61 unique checkers active in Org 9.8.10. Org's source has
62 checker registration calls because `LaTeX-$` is registered twice. The later
`LaTeX-$` checker replaces the earlier one; the active row below describes that
later ambiguous-dollar check. The shadowed checker is not dispatched.

| Org checker | Classification | Status | Check |
|---|---|---|---|
| `misplaced-heading` | Document only | Implemented | Heading syntax that appears after text on the same line. |
| `duplicate-custom-id` | Document only | Implemented | Repeated `CUSTOM_ID` values. |
| `duplicate-name` | Document only | Implemented | Repeated `NAME` values. |
| `duplicate-target` | Document only | Implemented | Repeated Org targets. |
| `duplicate-footnote-definition` | Document only | Implemented | Repeated footnote labels. |
| `orphaned-affiliated-keywords` | Document only | Implemented | Affiliated keywords with no element. |
| `combining-keywords-with-affiliated` | Document only | Implemented | Independent keywords immediately before affiliated keywords. |
| `obsolete-affiliated-keywords` | Document only | Implemented | Deprecated affiliated keyword names. |
| `deprecated-export-blocks` | Document only | Implemented | Deprecated backend-specific export block syntax. |
| `deprecated-header-syntax` | Caller material | Implemented | Deprecated Babel header properties; requires `:babel-header-arguments`. |
| `missing-language-in-src-block` | Document only | Implemented | Source blocks without a language. |
| `suspicious-language-in-src-block` | Caller material | Implemented | Source block languages absent from `:source-languages`. |
| `missing-backend-in-export-block` | Document only | Implemented | Export blocks without a backend. |
| `invalid-babel-call-block` | Document only | Implemented | Invalid Babel call syntax or end headers. |
| `wrong-header-argument` | Babel parser dependency | Dependency wait | Invalid Babel header argument names. |
| `wrong-header-value` | Babel parser dependency | Dependency wait | Invalid Babel header values and combinations. |
| `named-result` | Babel parser dependency | Dependency wait | Named source blocks whose result links will not export. |
| `empty-header-argument` | Babel parser dependency | Dependency wait | Empty Babel header values. |
| `deprecated-category-setup` | Document only | Implemented | Misuse of the `CATEGORY` keyword. |
| `invalid-coderef-link` | Document only | Implemented | Code reference links with unknown destinations. |
| `invalid-custom-id-link` | Document only | Implemented | Custom ID links with unknown destinations. |
| `invalid-fuzzy-link` | Document only | Implemented | Fuzzy links with unknown document locations. |
| `invalid-id-link` | Caller material | Implemented | ID links absent from `:known-ids`. |
| `trailing-bracket-after-link` | Document only | Implemented | A possibly confusing `]` after a link. |
| `unclosed-brackets-in-link-description` | Document only | Implemented | An unmatched `[` inside a link description. |
| `link-to-local-file` | Caller material | Implemented | Local links or images rejected by `:file-exists-p`. |
| `non-existent-setupfile-parameter` | Caller material | Implemented | Setup files rejected by `:file-exists-p`. |
| `wrong-include-link-parameter` | Caller material | Implemented | Include paths rejected by `:file-exists-p`; optional search validation uses `:include-target-p`. |
| `obsolete-include-markup` | Document only | Implemented | Deprecated markup in `INCLUDE` keywords. |
| `unknown-options-item` | Caller material | Implemented | `OPTIONS` items absent from `:export-options`. |
| `misspelled-export-option` | Caller material | Implemented | Potentially misspelled export property names, using `:export-options`. |
| `invalid-macro-argument-and-template` | Caller material | Implemented | Macro arguments and templates, using `:macro-definitions` and `:macro-templates`. |
| `special-property-in-properties-drawer` | Document only | Implemented | Special properties placed in a properties drawer. |
| `obsolete-properties-drawer` | Document only | Implemented | Incorrect properties drawer contents or location. |
| `invalid-effort-property` | Document only | Implemented | Invalid `EFFORT` duration values. |
| `invalid-id-property` | Document only | Implemented | `ID` properties containing `::`. |
| `undefined-footnote-reference` | Document only | Implemented | Footnote references without definitions. |
| `unreferenced-footnote-definition` | Document only | Implemented | Footnote definitions with no references. |
| `extraneous-element-in-footnote-section` | Document only | Implemented | Non-footnote content in a footnote section. |
| `invalid-keyword-syntax` | Document only | Implemented | Lines that may be misspelled keywords. |
| `invalid-image-alignment` | Document only | Implemented | Unsupported image alignment attributes. |
| `invalid-block` | Document only | Implemented | Invalid block closers or incomplete blocks. |
| `mismatched-planning-repeaters` | Document only | Implemented | Different repeaters in `SCHEDULED` and `DEADLINE`. |
| `misplaced-planning-info` | Document only | Implemented | Planning information in a misplaced line. |
| `incomplete-drawer` | Document only | Implemented | Likely incomplete or misleading drawers. |
| `indented-diary-sexp` | Document only | Implemented | Indented diary sexps. |
| `quote-section` | Document only | Implemented | Deprecated headlines and inline tasks named `QUOTE ...` or `COMMENT QUOTE ...`. |
| `file-application` | Document only | Implemented | Deprecated `file+application` links. |
| `percent-encoding-link-escape` | Document only | Implemented | Links using obsolete percent-encoding escapes. |
| `spurious-colons` | Document only | Implemented | Tags containing an extra colon. |
| `non-existent-bibliography` | Caller material | Implemented | Bibliography paths rejected by `:file-exists-p`. |
| `missing-print-bibliography` | Document only | Implemented | Citations with no `PRINT_BIBLIOGRAPHY` keyword. |
| `invalid-cite-export-declaration` | Caller material | Implemented | Cite export processors absent from `:cite-export-processors`. |
| `incomplete-citation` | Document only | Implemented | Incomplete citation markup. |
| `item-number` | Document only | Implemented | Inconsistent ordered list counters. |
| `priority` | Document only | Implemented | Invalid, malformed, or out-of-bounds priorities. |
| `LaTeX-$` | Document only | Implemented | Dollar signs that may be mistaken for LaTeX fragment boundaries. |
| `beamer-frame` | Caller material | Implemented | Frame environment text, using `:beamer-frame-environment`. |
| `timestamp-syntax` | Document only | Implemented | Potentially malformed timestamps. |
| `clock-syntax` | Document only | Implemented | Potentially malformed clock lines. |
| `planning-inactive` | Document only | Implemented | Inactive timestamps in `SCHEDULED` or `DEADLINE`. |

The four Babel parser-dependent checkers are intentionally not dispatched until
the parser provides structured header arguments. Their existing Org 9.8.10
names remain in the inventory so the deferred coverage is explicit.

## Caller context

Pass only the material relevant to the checks you select. Empty collections are
valid supplied data when they represent an empty caller registry or index. The
`org-lint` dispatcher skips a checker when any required key is absent.

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
directory and attachments without making the library assume a process working
directory. `:include-target-p` is a two-argument predicate receiving the
include file path and search part; it checks a search target against
caller-supplied included-file contents. If it is absent, only the path checks
run. `:source-languages`, `:known-ids`, and
`:cite-export-processors` are lists of strings. `:babel-header-arguments` is a
list of argument-name strings; a leading colon is optional. `:export-options`
is a list of option names. Backend-specific property names use `backend:KEY`,
such as `html:HTML_HEAD`. `:macro-definitions` and `:macro-templates` are alists
from string macro names to string templates. `:beamer-frame-environment` is the
configured environment name as a string.

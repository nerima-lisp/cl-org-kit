# Export

`export-org` renders a parsed document as HTML, GitHub Flavored Markdown (GFM),
ASCII, or LaTeX source. The default output type is a string.

Evaluate these forms in a Common Lisp REPL after installing `cl-org-kit`.

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil "#+TITLE: Release notes~%~%* Version 0.1.0~%A *small* release.~%")))
       (context (org-kit:make-export-context :title "Project release notes")))
  (list
   (org-kit:export-org document :html :context context)
   (org-kit:export-org document :gfm :context context)
   (org-kit:export-org document :ascii :context context)
   (org-kit:export-org document :latex :context context)
   (org-kit:export-org document :gfm
                       :context context :output-type :octets)))
~~~

LaTeX export returns source only; it does not invoke a TeX engine or generate a
PDF. By default it uses the `article` class with `[11pt]` options, ignores
`#+LATEX_CLASS`, `#+LATEX_CLASS_OPTIONS`, and `#+LATEX_HEADER`, and omits raw
LaTeX body constructs. These include `#+LATEX:` lines, export blocks, snippets
such as `@@latex:\emph{raw}@@`, LaTeX fragments such as `\(...\)`, and LaTeX
environments. Set `:allow-raw-latex t` in `export-org` to pass them through.
With that option, `#+LATEX_CLASS` selects the class,
`#+LATEX_CLASS_OPTIONS` sets its options, and `#+LATEX_HEADER` values are
inserted after the default packages. Both `#+BEGIN_EXPORT latex` and
`#+BEGIN_EXPORT tex` pass their contents through only when this option is true.
Enable raw LaTeX only for trusted documents: commands such as `\input` can read
local files, subject to TeX's `openin_any` setting, and `\write18` may run
shell commands depending on the engine and its shell-escape configuration.

The LaTeX backend uses `verbatim` for code, example, fixed-width, and dynamic
blocks; a body containing `\end{verbatim}` signals `org-export-error`.
`listings` and `minted` are not supported. A local image is included only when
its extension is png, jpg, jpeg, gif, svg, or pdf (case-insensitive). Its path
must use only ASCII letters and digits plus `.`, `_`, `-`, and `/`. It must be
relative and contain no `..` component. Unsafe image paths fall back to the link
label. The `..` restriction applies only to image inclusion. The separate URL
check for non-image file links permits relative destinations, including `..`
paths. Accepted external (`http`, `https`, `mailto`) and
non-image file links render as `\href`; rejected ones fall back to their label.
For a link without a description, the label comes from its parsed path and may
differ from the destination. Internal links use `\hyperref` when their target
resolves and otherwise render their label.
`ATTR_LATEX` values are comments, so `:options` and `:environment` do not affect
rendering. A labeled footnote reference such as `[fn:a]` uses a
matching definition such as `[fn:a] Note text.`; repeated references outside a
footnote render the definition again, while references inside a footnote render
as `\footnotemark`. With the default `:inlinetask-min-level 15` setting,
`*************** TODO Review` is parsed as an inlinetask and the heading
renderer maps its 15-star level to `subparagraph`. This setting controls which
star counts are classified as inlinetasks; the star count determines heading
level. It is starred when heading numbering is disabled.

With `:output-type :octets`, export returns a UTF-8 octet vector. With
`:output-type :stream`, supply an output stream with `:stream`. Character streams
receive text; streams whose element type is
`(unsigned-byte 8)` receive UTF-8 octets. Other stream element types are
rejected, and a successful call returns the stream.

`make-export-context` accepts `:title`, `:author`, `:date`, `:language`, and
`:time-context`. Non-NIL title, author, date, and language values override
`#+TITLE`, `#+AUTHOR`, `#+DATE`, and `#+LANGUAGE`; NIL leaves the in-document
value in effect. `:time-context` accepts an `org-time-context` value and is
separate from Org keyword metadata.

## Source blocks

A source block's `:exports` header argument selects what is exported, and
defaults to `code`. `code` exports the block, `results` exports the
`#+RESULTS:` element that follows it with only blank lines between, `both`
exports the two, and `none` exports neither. Source blocks are not executed,
so `results` exports nothing when no such `#+RESULTS:` element exists.
Results that the mode leaves out take no part in footnote numbering.

## Errors and limits

Unsupported syntax for a backend signals `org-export-unsupported-syntax` with
the backend, source excerpt, position, and feature. Other export failures,
including an invalid backend or output configuration, signal
`org-export-error`. Invalid context values can signal `type-error`.

The document limits cap output at 64 Mi characters (`:max-output-length`) or
256 Mi UTF-8 octets (`:max-output-octets`). Export expansion is limited to
depth 32 (`:max-export-expansion-depth`), 64 Mi expanded characters
(`:max-export-expanded-characters`), and 4 Mi expanded nodes
(`:max-export-expanded-nodes`) by default. Set these options on
`make-limits` and pass the result to `parse-org`. Exceeding a configured
ceiling signals `org-resource-limit-error`.

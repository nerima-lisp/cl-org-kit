# Export

`export-org` renders a parsed document as HTML, GitHub Flavored Markdown (GFM),
ASCII, or LaTeX source. The backend selects the document format. `:output-type`
selects its representation: a string by default, UTF-8 octets, or a stream.

In this repository, `nix develop --command sbcl` opens a REPL with the pinned
dependencies available. For another ASDF setup, make `cl-org-kit` and its
dependencies available as described in [Getting Started](../getting-started.md).

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
`#+LATEX_CLASS_OPTIONS` sets its options, and values from `#+LATEX_HEADER` and
`#+LATEX_HEADER_EXTRA` are inserted after the default packages, in that order.
Both `#+BEGIN_EXPORT latex` and
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
rendering.

A labeled footnote reference such as `[fn:a]` uses a matching definition such
as `[fn:a] Note text.`; repeated references outside a footnote render the
definition again, while references inside a footnote render as
`\footnotemark`.

The default `:inlinetask-min-level` is 15. Pass this keyword to `parse-org` to
change which star counts are classified as inlinetasks. For example,
`*************** TODO Review` is parsed as an inlinetask by default, and the
heading renderer maps its 15-star level to `subparagraph`. Setting
`#+OPTIONS: num:nil` renders this heading with a starred `\subparagraph*`
command.

With `:output-type :octets`, export returns a UTF-8 octet vector. With
`:output-type :stream`, supply an output stream with `:stream`. Character streams
receive text; streams whose element type is
`(unsigned-byte 8)` receive UTF-8 octets. Other stream element types are
rejected, and a successful call returns the stream.

`make-export-context` accepts `:title`, `:author`, `:date`, `:language`, and
`:time-context`. Non-NIL title, author, date, and language values override
`#+TITLE`, `#+AUTHOR`, `#+DATE`, and `#+LANGUAGE`; NIL leaves the in-document
value in effect. `:time-context` accepts an `org-time-context` value and is
separate from Org keyword metadata. It carries an explicit reference time, time
zone, and daylight-saving policy into callback metadata under `:time-context`.
A callback can read it with
`(getf (org-kit:export-render-context-metadata context) :time-context)`. See
[Clock](clock.md) for an example of constructing a time context.

Pass per-export extension data with `:extensions` when a callback needs
application-specific state. Keys must be keywords. The constructor and
`export-context-extensions` accessor copy the association-list structure, but
they do not copy its values. Keep those values unchanged until export finishes.
Read them with `export-context-extensions` on the caller context or
`export-render-context-extension` inside a renderer or filter.

## Registering backends and filters

`register-export-backend` adds a root backend with a body renderer.
`register-derived-export-backend` adds a backend that inherits from an
existing backend and overrides only the supplied fields. A root renderer can
render prepared views through `export-render-context-root-views` and
`export-render-view`. A node transcoder receives the node, its view, a render
frame, and the callback context. A view is the prepared representation of one
node and its prepared children. The context accessors are
`export-render-context-backend`, `export-render-context-owner-backend`,
`export-render-context-document`, `export-render-context-metadata`,
`export-render-context-options`, `export-render-context-limits`,
`export-render-context-root-views`, `export-render-context-toc`,
`export-render-context-footnotes`, `export-render-context-state`,
`export-render-context-raw-output-p`, and `export-render-context-extension`.
The backend accessor returns the requested backend; the owner accessor returns
the backend that defined the active callback. The state accessor returns state
for that callback owner, and the raw-output accessor checks whether a raw output
kind is allowed. Frame accessors are `export-render-frame-mode`,
`export-render-frame-table-cell-p`,
`export-render-frame-preserve-source-breaks-p`, and
`export-render-frame-output-offset`; the last reports the current output
position.

Set `:option-specifications` to add or override an Org `#+OPTIONS` token. Each
specification is a plist with `:token`, `:key`, and `:parser` fields and an
optional `:default`. For example, this accepts the `compact` token as a boolean
option and defaults it to `nil`:

~~~lisp
:option-specifications
(list (list :token "compact" :key :compact
            :parser :boolean :default nil))
~~~

A function parser receives the token's unparsed value, its source keyword
node, and the requested backend; its return value becomes the parsed option.
Read an option in a renderer with
`(org-kit:export-option-value context :compact)`. Defaults may be `nil`,
symbols, numbers, characters, strings, or acyclic cons and vector structures
made from those values. Strings are copied, and conses and vectors are
recursively copied when registered and when read. Hash tables, non-vector
arrays, structures, and other object types are rejected.

Root backends accept common Org export keywords and may add backend-specific
keywords with `:accepted-keywords`. `:retained-keywords` keeps accepted keyword
nodes in prepared views for later processing. The common citation keywords
`CITE_EXPORT`, `BIBLIOGRAPHY`, and `PRINT_BIBLIOGRAPHY` are retained by default.

~~~lisp
(org-kit:register-export-backend
 :plain
 :renderer
 (lambda (context)
   (with-output-to-string (output)
     (dolist (view (org-kit:export-render-context-root-views context))
       (write-string (org-kit:export-render-view context view) output))))
 :transcoders
 (list
  (cons :plain-text
        (lambda (node view frame context)
          (declare (ignore view frame context))
          (org-kit:node-text node)))))

(org-kit:register-derived-export-backend
 :labelled-plain :plain
 :filters
 (list
  (cons :final-output
        (list (lambda (text context)
                (declare (ignore context))
                (concatenate 'string "Custom: " text))))))

(let ((document (org-kit:parse-org "Hello, world.")))
  (unwind-protect
       (org-kit:export-org document :labelled-plain)
    (org-kit:unregister-export-backend :labelled-plain)
    (org-kit:unregister-export-backend :plain)))
~~~

Register filters with `:filters` when adding or deriving a backend. The value
maps stages to ordered callback lists, and each callback receives `(value
context)`.
Stages run in this order: `:options`, `:parse-tree`, `:plain-text`,
`(:element node-type)`, `:body`, and `:final-output`. Within a stage, a derived
backend's callbacks run before its parent's callbacks, and callbacks keep their
listed order. Callback values are:

| Stage | Value |
|---|---|
| `:options` | The explicit option alist; defaults are read separately with `export-option-value`. |
| `:parse-tree` | The list of prepared root `export-view` values. |
| `:plain-text` | The rendered string for a plain-text node. |
| `(:element node-type)` | The rendered string for one node. |
| `:body` | The body string after rendering. |
| `:final-output` | The complete output string. |

A filter that returns `nil` leaves its input unchanged and lets the next
callback run. An empty string is a valid result and stops the remaining
callbacks in that stage. `node-post-blank` counts blank lines after an Org
node. The backend applies its output spacing for that value before the element
filter runs, so the filter receives that spacing too.

Use `register-export-link-type` to add a handler for one link type on a
backend. The handler receives the link path, rendered description, render
frame, and callback context. It returns a `make-export-link-result` value with
a `:link` or `:image` kind, a destination, and a description. The backend
applies its URL or file-path checks and output escaping to that result. For
example, register a handler for links such as `[[issue:123][Fix the bug]]`:

~~~lisp
(unwind-protect
     (progn
       (org-kit:register-export-link-type
        :html "issue"
        (lambda (path description frame context)
          (declare (ignore frame context))
          (org-kit:make-export-link-result
           :kind :link
           :destination (concatenate 'string "https://issues.example.test/" path)
           :description description)))
       (org-kit:export-org
        (org-kit:parse-org "[[issue:123][Fix the bug]]")
        :html))
  (org-kit:unregister-export-link-type :html "issue"))
~~~

Call `unregister-export-backend` to remove a custom backend after removing its
derived backends. `export-backend-derived-p` reports strict ancestry, so a
backend is not derived from itself. Built-in backends cannot be unregistered.

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

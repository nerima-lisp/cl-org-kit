# Export

`export-org` renders a parsed document as HTML, GitHub Flavored Markdown (GFM),
ASCII, LaTeX, iCalendar, Texinfo, or man output. The backend selects the
document format. `:output-type` selects its representation: a string by default,
UTF-8 octets, or a stream. `export-org-texinfo` and `export-org-man` are
convenience entrypoints for the Texinfo and man backends.

In this repository, `nix develop --command sbcl` opens a REPL with the pinned
dependencies available. For another ASDF setup, make `cl-org-kit` and its
dependencies available as described in [Getting Started](../getting-started.md).

~~~lisp
(require :asdf)
(asdf:load-system "cl-org-kit")

(let* ((document
         (org-kit:parse-org
          (format nil "#+TITLE: Project release notes~%~%* Version 0.1.0~%A *small* release.~%")))
       (context (org-kit:make-export-context :title "Project release notes")))
  (list
   (org-kit:export-org document :html :context context)
   (org-kit:export-org document :gfm :context context)
   (org-kit:export-org document :ascii :context context)
   (org-kit:export-org document :latex :context context)
   (org-kit:export-org-texinfo document :context context)
   (org-kit:export-org-man document :context context)
   (org-kit:export-org document :gfm
                       :context context :output-type :octets)))
~~~

Texinfo export returns `.texi` source. It does not run `makeinfo` or create an
`.info` file. Man export returns roff source for a man page; it does not invoke
a roff formatter.

The export context's title overrides the document's `#+TITLE` for this export.

## Includes

`export-org` does not open files named by `#+INCLUDE`. Pass an
`:include-resolver` function to supply each included source as a string. The
function receives the include path and the including document's source name,
which is `nil` when that document has no source name. It must return a string.
The path argument excludes an optional `::target` and include options, so the
resolver can choose how to look up the source.

~~~lisp
(let ((document
        (org-kit:parse-org "#+INCLUDE: intro.org"
                           :source-name "main.org")))
  (org-kit:export-org
   document :html
   :include-resolver
   (lambda (path parent-source)
     (cond
       ((and (string= path "intro.org")
             (string= parent-source "main.org"))
        (format nil "* Introduction~%Supplied by caller."))
       (t (error "Unexpected include ~A from ~A" path parent-source))))))
~~~

The following directives show syntax; each path must be handled by your
resolver.

~~~org
#+INCLUDE: "chapters.org::*Introduction" :lines "2-6" :minlevel 2
#+INCLUDE: "chapters.org::#stable-id"
#+INCLUDE: "notes.org::named-note"
#+INCLUDE: "snippets.org" src lisp
#+INCLUDE: "examples.org" example
#+INCLUDE: "raw.html" export html
~~~

Include targets can select a heading, a heading's `CUSTOM_ID` with `#id`, or a
named element declared with `#+NAME`. Without a target, `:lines "2-4"`
includes file lines 2 and 3, stopping before line 4. Org resolves a
`::target` first, then computes the range relative to that selected element.
For a selected heading, `:lines "2-6"` includes target-relative lines 2
through 6 and stops before line 7. In both cases, the end position is
exclusive. `:minlevel N` sets the shallowest included heading level; when
omitted, Org places the included headings one level below the nearest containing
heading, or at level 1 at the document top level.

The `src LANG`, `example`, and `export BACKEND` wrappers include the supplied
text as literal block contents. `:minlevel` does not change wrapped text.
Nested includes use the same resolver, with each including path passed as the
parent source name. An include cycle signals an `org-export-error` with reason
`:include-cycle`; a missing target signals reason `:include-target-not-found`.

`#+SETUPFILE` uses the same resolver. The resolver receives the setup path and
the source name of the document containing the directive; for a nested setup
file, that source name is the parent setup path. The library collects export
keywords from the supplied text at the directive's position, following nested
`#+SETUPFILE` directives in order. Setup file body text is not added to the
document. A missing resolver, a non-string result, or a setup file cycle signals
an `org-export-error` with reason `:missing-setupfile-resolver`,
`:invalid-setupfile-result`, or `:setupfile-cycle`. Exceeding
`:max-export-expansion-depth` signals an `org-resource-limit-error`.

## Backend keywords, options, and tables of contents

The HTML and LaTeX backends accept the backend-specific keywords and
`#+OPTIONS` tokens defined by Org 9.8.10. They aggregate repeated keyword
values using the backend's Org rules and expose the resulting values through
`export-render-context-options` as an association list. In a backend callback,
read a value with `(cdr (assoc :html-doctype (org-kit:export-render-context-options context)))`.
For example,
`HTML_DOCTYPE` maps to `:html-doctype`, `LATEX_FOOTNOTE_COMMAND` maps to
`:latex-default-footnote-command`, and `html-style` maps to
`:html-head-include-default-style`.

The HTML backend applies `#+HTML_CONTENT_CLASS` to its content wrapper. It
accepts `HTML_DOCTYPE`, `HTML_CONTAINER`, `HTML_LINK_HOME`, `HTML_LINK_UP`,
`HTML_EQUATION_REFERENCE_FORMAT`, `HTML_HEAD`, `HTML_HEAD_EXTRA`, `INFOJS_OPT`,
and `LATEX_HEADER`, along with the shared `DESCRIPTION`, `KEYWORDS`,
`SUBTITLE`, and `CREATOR` keywords. It also parses the `html5-fancy`,
`html-link-use-abs-url`, `html-postamble`, `html-preamble`, `html-style`,
`html-scripts`, and `html-self-link-headlines` options. The LaTeX backend uses
`LATEX_CLASS`, `LATEX_CLASS_OPTIONS`, `LATEX_HEADER`, and
`LATEX_HEADER_EXTRA` when `:allow-raw-latex` is true. It also accepts
`LATEX_CLASS_PRE`, `LATEX_FOOTNOTE_COMMAND`, `LATEX_ENGRAVED_THEME`,
`LATEX_COMPILER`, `DESCRIPTION`, `KEYWORDS`, `SUBTITLE`, and `DATE`, and parses
the `latex-use-sans` option. Accepted values that the built-in renderer does
not use remain available in the render context for backend consumers.

`#+OPTIONS: toc:N` sets the depth of the document-wide table of contents;
`toc:nil` disables it. A `#+TOC: headlines N` directive adds a headline table
of contents at its position in the document. `#+TOC: headlines local` limits
the entries to the nearest containing headline. The standalone `#+TOC: local`
directive is accepted by Org 9.8.10 but emits no table of contents.
`#+TOC: tables` and `#+TOC: listings` request lists of tables and source
listings.
HTML, LaTeX, and ASCII render headline TOCs, local headline TOCs, table lists,
and listing lists. GFM renders headline and local headline TOCs; Org 9.8.10
emits no output for its table and listing modes. An unsupported mode signals
`org-export-error` with reason `:toc-mode`.

## LaTeX

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
Enable raw LaTeX only for trusted documents. When a downstream TeX engine
processes the generated source, commands such as `\input` can read local
files, subject to TeX's `openin_any` setting, and `\write18` may run shell
commands depending on the engine and its shell-escape configuration.

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

## Footnotes

A labeled footnote reference such as `[fn:a]` uses a matching definition such
as `[fn:a] Note text.`; repeated references outside a footnote render the
definition again, while references inside a footnote render as
`\footnotemark`.

## Inlinetasks

The default `:inlinetask-min-level` is 15. Pass this keyword to `parse-org` to
change which star counts are classified as inlinetasks. For example,
`*************** TODO Review` is parsed as an inlinetask by default. In the
LaTeX backend, the heading renderer maps its 15-star level to `subparagraph`.
Setting `#+OPTIONS: num:nil` renders this heading with a starred
`\subparagraph*` command.

## Output types

With `:output-type :octets`, export returns a UTF-8 octet vector. With
`:output-type :stream`, supply an output stream with `:stream`. Character streams
receive text; streams whose element type is
`(unsigned-byte 8)` receive UTF-8 octets. Other stream element types are
rejected, and a successful call returns the stream.

## Export contexts and time macros

`make-export-context` accepts `:title`, `:author`, `:date`, `:language`, and
`:time-context`. Non-NIL title, author, date, and language values override
`#+TITLE`, `#+AUTHOR`, `#+DATE`, and `#+LANGUAGE`; NIL leaves the in-document
value in effect. `:time-context` accepts an `org-time-context` value and is
separate from Org keyword metadata. It carries an explicit reference time, time
zone, and daylight-saving policy into callback metadata under `:time-context`.
A renderer or filter callback can read it with
`(getf (org-kit:export-render-context-metadata render-context) :time-context)`. See
[Clock](clock.md) for an example of constructing a time context.

Pass per-export extension data with `:extensions` when a callback needs
application-specific state. Keys must be keywords. The constructor and
`export-context-extensions` accessor copy the association-list structure, but
they do not copy its values. Keep those values unchanged until export finishes.
Read them with `export-context-extensions` on the caller context or
`export-render-context-extension` inside a renderer or filter.
The built-in `modification-time` macro formats caller-supplied
`org-time-context` values. Its first argument is the format string. An optional
second argument containing non-whitespace text requests the version-control
modification time. Put
the file modification context under `:modification-time` and the version-control
context under `:modification-time-vc` in `:extensions`:

~~~lisp
(let* ((file-time-context
         (org-kit:make-time-context
          :reference-time (org-kit:make-org-time-value "2030-01-02T03:04:05")
          :zone (org-kit:make-fixed-time-zone 0)))
       (vc-time-context
         (org-kit:make-time-context
          :reference-time (org-kit:make-org-time-value "2031-03-04T05:06:07")
          :zone (org-kit:make-fixed-time-zone 0)))
       (context
         (org-kit:make-export-context
          :extensions (list (cons :modification-time file-time-context)
                            (cons :modification-time-vc vc-time-context))))
       (document
         (org-kit:parse-org "{{{modification-time(%F,vc)}}}")))
  (org-kit:export-org document :ascii :context context))
~~~

Pass `file-time-context` as `:time-context` as well when formatting `time`
macros; `(org-kit:make-export-context :time-context file-time-context)` makes
`{{{time(%F)}}}` expand to `2030-01-02`.

Here, `{{{modification-time(%F,vc)}}}` expands to `2031-03-04`. A second
argument containing non-whitespace text selects `:modification-time-vc` when
that context is supplied; otherwise it falls back to `:modification-time`
(`2030-01-02`). A missing or whitespace-only second argument uses
`:modification-time`. When `:modification-time` has no value, the macro expands
to an empty string regardless of the arguments. The library does not read file
modification times or query version control.

`time` and `modification-time` accept these format directives: `%%`, `%Y`,
`%y`, `%C`, `%m`, `%d`, `%e`, `%H`, `%k`, `%I`, `%l`, `%M`, `%S`, `%j`, `%a`,
`%A`, `%b`, `%h`, `%B`, `%p`, `%P`, `%w`, `%u`, `%V`, `%G`, `%g`, `%U`, `%W`,
`%z`, `%Z`, `%s`, `%D`, `%F`, `%R`, `%T`, `%r`, `%c`, `%x`, `%X`, `%n`, and
`%t`. One modifier may follow `%` before a directive: `_` pads numeric output
with spaces, `-` removes numeric padding, `0` uses zero padding, and `^`
uppercases text output. A trailing `%` is retained literally. Unsupported
directives signal `org-export-unsupported-syntax` with feature
`:unsupported-time-format`.

`time` requires a `:time-context` and a format argument. Without the context,
it signals `org-export-unsupported-syntax` with feature
`:macro-context-required`; without the argument, it signals the same condition
with feature `:macro-format-required`. `modification-time` requires a format
argument when the `:modification-time` extension is present. With that
extension, omitting the argument signals `org-export-unsupported-syntax` with
feature `:macro-format-required`. Without the extension, it expands to an
empty string regardless of whether a format argument was supplied. These
omitted-argument conditions match Org 9.8.10's required first argument.

## iCalendar

Use `:icalendar` with `export-org` for one parsed document. Supply a time
context for `DTSTAMP`. A heading uses the `ID` in its own `:PROPERTIES:` drawer
for its UID; otherwise, it needs a random source through the context extension
`:icalendar-random-source`. Keep IDs unique across documents combined into one
calendar. The source is a zero-argument function that returns one octet from 0
through 255. This example supplies a deterministic source that returns
successive octets:

~~~lisp
(let* ((document
         (org-kit:parse-org
          (format nil "* Review~%<2026-10-22 Thu 09:00-10:00>~%")))
       (context
         (org-kit:make-export-context
          :time-context
          (org-kit:make-time-context
           :reference-time (org-kit:make-org-time-value "2026-10-21T12:00:00Z")
           :zone (org-kit:resolve-time-zone "UTC"))
          :extensions
          (let ((octet 0))
            (list (cons :icalendar-random-source
                        (lambda ()
                          (prog1 octet
                            (setf octet (mod (1+ octet) 256)))))))))
  (org-kit:export-org document :icalendar :context context))
~~~

`export-icalendar-documents` combines a proper list of already parsed
documents into one `VCALENDAR`. Its signature is:

~~~lisp
(org-kit:export-icalendar-documents documents
  &key context random-source options (output-type :string) stream)
~~~

`context` is an `export-context` carrying the time context and optional title
or author. `random-source` takes precedence over the context extension. The
`options` argument is an iCalendar option plist. It overrides in-buffer
`#+ICALENDAR_*` keywords, while an `ICALENDAR_*` property in a heading's own
drawer overrides the option for that heading's events. Precedence is defaults,
keywords, call options, then heading properties. `ID` and `ICALENDAR_*`
properties are read only from that heading's own `:PROPERTIES:` drawer; they
are not inherited from parent headings or document `#+PROPERTY` values. For
`export-org`, pass the same option and random-source values with
`:icalendar-options` and `:icalendar-random-source` entries in
`make-export-context :extensions`.

The complete set of option keys is `:use-deadline`, `:use-scheduled`,
`:include-todo`, `:alarm-time`, `:force-alarm`, `:exclude-tags`,
`:summary-prefix-scheduled`, `:summary-prefix-deadline`, `:categories`,
`:with-timestamps`, `:todo-unscheduled-start`, `:include-sexps`,
`:include-body`, `:store-uid`, `:timezone`, `:date-time-format`, `:ttl`,
`:combined-name`, and `:description`. Their in-buffer forms use the
`#+ICALENDAR_<KEY>: value` spelling, with hyphens changed to underscores.
`#+ICAL-TTL` is also accepted as an alias for `#+ICALENDAR_TTL`, and Org's
`#+BIND` can set an `org-icalendar-<option>` value. The backend does not persist
generated UIDs. `:store-uid` defaults to `nil`; setting it to `t` signals
`org-export-error` with reason `:unsupported-option`, because Org's behavior
would write to the source file. Callers that need stable UIDs can set unique
local `ID` properties before export. `:exclude-tags` omits headings carrying
any listed tag. `:ttl` writes `X-PUBLISHED-TTL`; `:combined-name` sets
`X-WR-CALNAME`; and `:description` sets `X-WR-CALDESC`, which otherwise uses
the document title or an empty string.

~~~lisp
(let* ((time-context
         (org-kit:make-time-context
          :reference-time (org-kit:make-org-time-value "2026-10-21T12:00:00Z")
          :zone (org-kit:resolve-time-zone "UTC")))
       (documents
         (list (org-kit:parse-org
                (format nil "* First~%:PROPERTIES:~%:ID: first~%:END:~%<2026-10-22 Thu 09:00-10:00>~%"))
               (org-kit:parse-org
                (format nil "* Second~%:PROPERTIES:~%:ID: second~%:END:~%<2026-10-23 Fri 09:00-10:00>~%")))))
  (org-kit:export-icalendar-documents
   documents :context (org-kit:make-export-context :time-context time-context)
   :options '(:include-todo :all) :output-type :octets))
~~~

The default `:with-timestamps` value is `:active`. Use `:inactive` for
inactive timestamps, `:all` for active and inactive timestamps, or `nil` for no
timestamps. `:active-exclude-diary` also selects active timestamps; diary
sexps are handled separately and are never evaluated. The default
`:use-scheduled` value is `(:todo-start)`, which creates `DTSTART` for TODO
entries. `:use-deadline` defaults to `(:event-if-not-todo :todo-due)`, which
exports non-TODO deadlines as events and TODO deadlines as `DUE`.
`:use-scheduled` accepts `:event-if-not-todo`, `:event-if-todo`,
`:event-if-todo-not-done`, and `:todo-start`; `:use-deadline` accepts those
three event selectors and `:todo-due`. Include multiple selectors in a list to
combine their effects, for example pass
`:options '(:use-scheduled (:event-if-not-todo :todo-start))`.
`:include-todo` defaults to `nil`; `t` includes unfinished TODO entries,
`:all` includes TODO and DONE entries, `:unblocked` excludes a TODO with an
unfinished TODO child or an earlier TODO sibling under an `ORDERED` parent, and
a list selects the named TODO keywords. Categories default to local tags and
the document category. `:categories` accepts a list containing any of
`:local-tags`, `:category`, `:todo-state`, and `:all-tags`. Set the document
category with `#+CATEGORY: Planning`; if absent, the source filename without its
extension is used, then `OrgMode`. `:include-body` defaults to `t`: use `nil` to omit the
body, `t` to include all of it, or a nonnegative integer to limit it to that
many characters. A heading's body text is included in `DESCRIPTION`; a local
`DESCRIPTION` property overrides it. Scheduled and deadline events use the
`S: ` and `DL: ` summary prefixes; VTODO summaries have no prefix. Alarms are
disabled by default (`:alarm-time` is zero and `:force-alarm` is false). For a
timed event, a positive `APPT_WARNTIME` property supplies the alarm lead time in
minutes; otherwise a positive `:alarm-time` supplies the fallback lead time.
`:force-alarm t` allows a zero-minute `VALARM` when the selected lead time is
zero.

`:todo-unscheduled-start` defaults to `:recurring-deadline-warning`: an
unscheduled TODO with a recurring deadline starts at its deadline warning date,
14 days earlier unless the timestamp supplies a warning interval. Set it to
`:deadline-warning` to use any deadline, or `:current-datetime` to use the
reference instant in the supplied time context. The latter requires a time
context. `nil` omits the generated start.

`:timezone` sets `X-WR-TIMEZONE` and supplies `%Z` in a custom date-time
format. The default `:date-time-format` is `:%Y%m%dT%H%M%S`. The colon
separates an optional property prefix from the timestamp pattern and is
required even when the prefix is empty. Supported
directives are `%Y`, `%y`, `%m`, `%d`, `%H`, `%M`, `%S`, `%Z`, and `%%`; for
example, `;TZID=%Z:%Y%m%dT%H%M%S` writes a TZID parameter. A heading's local
`TIMEZONE` property takes precedence for date-time timestamps: `UTC` converts
them to UTC and appends `Z`; any other value is written as `TZID=<value>` while
preserving the local fields. Date-only timestamps remain `VALUE=DATE` without a
TZID parameter, except that `TIMEZONE: UTC` interprets local midnight using the
zone and daylight-saving policy in the caller's time context, then converts it
to a UTC date-time.

Org's `include-sexps` default is enabled. This backend recognizes literal
`diary-block` date ranges as data, without evaluating the expression. For
example, `%%(diary-block 1 2 2025 1 4 2025) Literal block` uses arguments in
day, month, year, day, month, year order and emits a daily recurrence from
January 2 through January 4, 2025. Other diary sexps outside
this supported subset signal `org-export-unsupported-syntax` with the
`:diary-sexp` feature. Set `:include-sexps nil` to omit all diary sexps.

Active timestamp ranges retain their explicit end. A point timestamp receives
the Org default duration of two hours, or one day for a date-only timestamp.
Repeaters become `RRULE` values. Text values escape backslashes, commas,
semicolons, and line breaks. Output uses CRLF and folds physical lines at 75
UTF-8 octets without splitting a multibyte character. This octet limit follows
RFC 5545; Org 9.8.10 counts characters instead. For example, 74 ASCII bytes
followed by a two-byte UTF-8 character may make Org's first physical line 76
octets, while this backend folds before that character to stay within 75.

## Built-in macro values

The `title` and `author` macros use effective metadata, including caller
context overrides. The `email` and `keyword` macros read effective keyword
values, including values supplied through `#+SETUPFILE`. The `date` macro reads
the effective `#+DATE` keyword, including a value supplied through
`#+SETUPFILE`; an export context's `:date` override changes render metadata but
does not change this macro value. If `#+DATE` contains one timestamp, the macro
formats it when given an argument, as in `{{{date(%Y-%m-%d)}}}`. Without an
argument it returns the timestamp in its source form. Org represents this
timestamp macro as a function macro, so it expands only when an `:evaluator` /
`:emacs-lisp` callback is registered; see [Function macros](#function-macros).

The `input-file` macro returns the basename of the source name supplied to
`parse-org`. If the document has no source name, it expands to an empty string.
The library does not read a file to resolve this value. The `property` macro
takes the property name first and an optional location second. Without a
location it reads the current headline; a location can be a headline title or
`#CUSTOM_ID`, as in `{{{property(VALUE,#custom-id)}}}`. The `n` macro maintains a
named counter: no action increments it, `-` keeps the current value, an integer
sets it, and any other nonempty action resets it to 1.

`$N` in a macro template selects the Nth argument. `$0` and `$000` select the
first argument, and an index beyond the supplied arguments expands to an empty
string. A dollar sign followed by a non-digit remains literal. The default
`results` macro expands to its first argument, or to an empty string when it
has no argument. A local `#+MACRO: results ...` definition replaces that
default.

## Function macros

Org function macros use a template beginning with `(eval`, for example
`#+MACRO: greet (eval (concat "Hello, " $1))`. `export-org` passes the full
template and a context plist to the registered `:evaluator` / `:emacs-lisp`
callback. The plist contains the lowercase macro name, a copied list of parsed
argument strings, and the effective export context under `:macro-name`,
`:arguments`, and `:export-context`. The library does not parse or evaluate the
expression or substitute arguments into its body; the callback owns those
steps.

An evaluator can execute code supplied through an Org document. Register one
only for trusted input, or use a callback that applies its own sandbox and
resource limits.

Register an evaluator explicitly and pass its registry to `export-org`:

~~~lisp
(let ((registry (org-kit:make-callback-registry)))
  (org-kit:register-callback
   registry :evaluator :emacs-lisp
   (lambda (expression macro-context)
     (if (string= expression "(eval (concat \"Hello, \" $1))")
         (format nil "Hello, ~A"
                 (first (getf macro-context :arguments)))
         (error "Unsupported function macro template"))))
  (org-kit:export-org
   (org-kit:parse-org
    (format nil "#+MACRO: greet (eval (concat \"Hello, \" $1))~%~%{{{greet(World)}}}"))
   :ascii :callback-registry registry))
~~~

The callback receives the full template unchanged and the call's arguments in
order; an omitted argument list is `nil`, and an explicit empty argument is an
empty string. A `nil` callback result expands to an empty string, strings pass
through, and other values are converted with Common Lisp `~A`. If no evaluator
is registered, expansion signals `org-elisp-evaluator-error`. Conditions
signaled by a registered callback propagate unchanged.

## Registering backends and filters

`register-export-backend` adds a root backend with a body renderer.
`register-derived-export-backend` adds a backend that inherits from an
existing backend and overrides only the supplied fields. Callback fields use
the first non-`nil` value found from the child toward its ancestors. For keyed
maps such as `:transcoders`, `:node-policies`, `:option-specifications`,
`:raw-output-options`, and `:link-type-handlers`, a child's entry wins when
the parent has the same key. Accepted and retained keyword sets combine
entries from the parent and child. Filter callbacks run from child to parent,
preserving each backend's listed order.

A root renderer can render prepared views through
`export-render-context-root-views` and
`export-render-view`. A node transcoder receives the node, its view, a render
frame, and the callback context. A view is the prepared representation of one
node and its prepared children. The frame describes the current render
position: mode (`:block`, `:inline`, or `:object`), whether it is in a table
cell, whether source line breaks are preserved, and the current output offset.
The context accessors are
`export-render-context-backend`, `export-render-context-owner-backend`,
`export-render-context-document`, `export-render-context-metadata`,
`export-render-context-options`, `export-render-context-limits`,
`export-render-context-root-views`, `export-render-context-toc`,
`export-render-context-footnotes`, `export-render-context-state`,
`export-render-context-raw-output-p`, and `export-render-context-extension`.
During a node-transcoder call, the owner accessor returns the backend that
defined that transcoder, its state accessor returns that backend's render
state, and the frame describes the current node render. Function-macro
evaluators receive the separate plist described above. The backend accessor
returns the requested backend, and the raw-output accessor checks whether a
raw output kind is allowed. Frame accessors are `export-render-frame-mode`,
`export-render-frame-table-cell-p`,
`export-render-frame-preserve-source-breaks-p`, and
`export-render-frame-output-offset`; the last reports the current output
position.

Set `:option-specifications` to add or override an Org `#+OPTIONS` token. Each
specification is a plist with `:token`, `:key`, and `:parser` fields and an
optional `:default`. Built-in parser names include `:boolean`, `:integer`,
`:boolean-integer`, `:toc`, `:creator-mode`, `:sub-superscript`,
`:disabled-only`, `:archive-mode`, `:latex-mode`, `:tag-mode`,
`:timestamp-range`, `:selection-list`, `:drawer-list`, and `:tasks-mode`; a
function parser can implement a custom value. For example, this accepts the
`compact` token as a boolean option and defaults it to `nil`:

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

Root backends accept `TITLE`, `AUTHOR`, `DATE`, `LANGUAGE`, `OPTIONS`,
`SELECT_TAGS`, `EXCLUDE_TAGS`, `MACRO`, `ATTR_HTML`, `ARCHIVE`, `COLUMNS`,
`PRIORITIES`, `CONSTANTS`, `EXPORT_FILE_NAME`, `DESCRIPTION`, `KEYWORDS`,
`SUBTITLE`, `EMAIL`, `CREATOR`, `INCLUDE`, `RESULTS`, `NAME`, `CAPTION`,
`HEADER`, `CITE_EXPORT`, `BIBLIOGRAPHY`, and `PRINT_BIBLIOGRAPHY`. Add
backend-specific keywords with `:accepted-keywords`. `:retained-keywords` keeps
accepted keyword nodes in prepared views for later processing. The three
citation keywords are retained by default.

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
context)`. Stages run in this order: `:options`, `:parse-tree`, `:plain-text`,
`(:element node-type)`, `:body`, and `:final-output`. Callback values are:

| Stage | Value |
|---|---|
| `:options` | The explicit option alist; defaults are read separately with `export-option-value`. |
| `:parse-tree` | The list of prepared root `export-view` values. |
| `:plain-text` | The rendered string for a plain-text node. |
| `(:element node-type)` | The rendered string for one node. |
| `:body` | The body string after rendering. |
| `:final-output` | The complete output string. |

Check a parse-tree value with `org-kit:export-view-p` before calling
`org-kit:export-view-node`, `org-kit:export-view-children`, or
`org-kit:export-view-source-document`.

A filter that returns `nil` leaves its input unchanged and lets the next
callback run. An empty string is a valid result and stops the remaining
callbacks in that stage. `node-post-blank` is parsed spacing metadata that
counts blank lines after an Org node; it is not an export option. HTML omits
that spacing from both the rendered node and its element-filter input. Other
backends append the spacing to the rendered string before the element filter:
inline objects receive spaces and block elements receive newlines.

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

The registry rejects a second registration for an existing backend name. It
also rejects a second link handler for the same type on one backend, compared
without regard to case. A child backend may register its own handler for a
type inherited from its parent.

To replace a custom backend under the same name, unregister its derived
backends from leaf to root, unregister and register the backend, then register
the derived backends again from root to leaf. A backend with registered
descendants cannot be unregistered. `export-backend-derived-p` reports strict
ancestry, so a backend is not derived from itself. Built-in backends cannot be
unregistered.

To register a custom link handler again for the same backend and type, call
`unregister-export-link-type` first, then call `register-export-link-type`.
This removes only that backend's local custom handler; built-in handlers cannot
be removed.

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
256 Mi UTF-8 octets (`:max-output-octets`); Mi means 1,048,576. Export
expansion is limited to
depth 32 (`:max-export-expansion-depth`), 64 Mi expanded characters
(`:max-export-expanded-characters`), and 4 Mi expanded nodes
(`:max-export-expanded-nodes`) by default. Set these options on
`make-limits` and pass the result to `parse-org`. Exceeding a configured
ceiling signals `org-resource-limit-error`.

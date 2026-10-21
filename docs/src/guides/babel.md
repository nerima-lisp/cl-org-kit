# Babel

The Babel APIs resolve headers and variables, expand noweb references, execute
blocks through registered executors, return result updates as edit results, and
build tangle write plans. They do not start external processes or apply tangle
plans; callers choose the executors and when to apply plans.

## Example document and source block

```lisp
(defparameter *babel-document*
  (org-kit:parse-org
   "#+NAME: sample-items
- red
- blue

#+NAME: subtotal
#+BEGIN_SRC emacs-lisp
42
#+END_SRC

#+NAME: subtotal-call
#+CALL: subtotal()

#+NAME: static-consumer
#+BEGIN_SRC emacs-lisp :var items=sample-items
(print items)
#+END_SRC

#+NAME: consumer
#+BEGIN_SRC emacs-lisp :var total=subtotal-call :noweb yes
(print total)
<<subtotal()>>
#+END_SRC"))

(defun named-src-block (document name)
  (let ((found nil))
    (org-kit:map-nodes
     (lambda (node)
       (when (and (eq (org-kit:node-type node) :src-block)
                  (equal (org-kit:node-property node :name) name))
         (setf found node)))
     (org-kit:document-root document))
    found))

(defparameter *static-consumer-block*
  (named-src-block *babel-document* "static-consumer"))

(defparameter *consumer-block*
  (named-src-block *babel-document* "consumer"))

(defparameter *subtotal-block*
  (named-src-block *babel-document* "subtotal"))

(defun first-node-of-type (document type)
  (let ((found nil))
    (org-kit:map-nodes
     (lambda (node)
       (when (and (null found) (eq (org-kit:node-type node) type))
         (setf found node)))
     (org-kit:document-root document))
    found))

(defparameter *subtotal-call*
  (first-node-of-type *babel-document* :babel-call))
```

## Header arguments

Pass `org-kit:resolve-babel-parameters` a document and one of its `:src-block` nodes.
It returns an alist of key-value pairs.
Keys are uppercase strings.
Values are strings, or `nil` for flags.

The effective order is defaults, generic `header-args` properties,
language-specific properties, affiliated `#+HEADER` arguments, and source-block
arguments. A later value replaces an earlier value.
The built-in values are `:session none`, `:results replace`, `:exports code`, `:cache no`, `:noweb no`, `:hlines no`, and `:tangle no`.
Heading inheritance is resolved within each property layer before the next
layer is applied, so an inherited language-specific value overrides an
inherited generic value.
The `+` suffix appends a property value to its layer.
`:var` assignments merge by variable name.
`:results` options have four independent groups:

| Group | Values |
|---|---|
| Format | `file`, `list`, `vector`, `table`, `scalar`, `verbatim` |
| Representation | `raw`, `html`, `latex`, `org`, `code`, `pp`, `drawer`, `link`, `graphics` |
| Handling | `replace`, `silent`, `none`, `discard`, `append`, `prepend` |
| Result channel | `output`, `value` |

Choose at most one value from each group. A later value replaces only an earlier value from the same group.
Header resolution preserves this option set. `update-babel-results` supports a
defined subset of formats and operations; it signals an error for parsed options
that its writer does not support.
For HTML, GFM, and ASCII export, `:exports` controls the source block and its results:

| `:exports` value | Rendered content |
|---|---|
| `code` | Source block |
| `results` | Results |
| `both` | Source block and results |
| `none` | Neither |

```org
* Parent
:PROPERTIES:
:header-args: :session parent-generic
:header-args:emacs-lisp: :session parent-language
:END:
** Child
:PROPERTIES:
:header-args:emacs-lisp+: :var child=1
:END:
#+NAME: inherited-block
#+BEGIN_SRC emacs-lisp
body
#+END_SRC
```

Bind the document and its named source block before resolving parameters:

```lisp
(defparameter *inheritance-document*
  (org-kit:parse-org
   (format nil
           "* Parent~%:PROPERTIES:~%:header-args: :session parent-generic~%:header-args:emacs-lisp: :session parent-language~%:END:~%** Child~%:PROPERTIES:~%:header-args:emacs-lisp+: :var child=1~%:END:~%#+NAME: inherited-block~%#+BEGIN_SRC emacs-lisp~%body~%#+END_SRC~%")))

(defparameter *inherited-block*
  (named-src-block *inheritance-document* "inherited-block"))

(org-kit:resolve-babel-parameters
 *inheritance-document* *inherited-block*)
```

The result resolves `SESSION` to `parent-language` and includes the `child=1`
variable.

```lisp
(org-kit:resolve-babel-parameters *babel-document* *static-consumer-block*)
```

The call returns raw header values.
Use `org-kit:resolve-babel-variables` to parse the block's `:var` assignments.

## Static `:var` values

`org-kit:resolve-babel-variables` returns an alist from variable names to Common Lisp values.
Numeric literals are signed decimal integers or decimal floating-point values with an optional `e` exponent, such as `42`, `-.25`, and `1.25e+2`; ratios such as `3/4` remain strings.
It also accepts double-quoted strings, named Org pipe tables, and named Org plain lists.
Numeric table cells become numbers.
Other unquoted table cells and list items remain strings.
A named Org plain list can be used when each item is empty or has exactly one paragraph child.

```lisp
(org-kit:resolve-babel-variables *babel-document* *static-consumer-block*)
;; => (("items" . ("red" "blue")))
```

A bare value such as `sample-items` names another Org object; a missing name signals `org-kit:org-babel-error`.
Reader syntax such as `(+ 1 2)` is rejected instead of being evaluated.
The library never evaluates a `:var` expression.
Use `:result-resolver` for executable references.
The callback receives `(block-name resolved-variable-alist)` and returns that block's result as a Common Lisp value.
It may return `nil`; signal an error from the callback when its lookup has no result.
```lisp
(defparameter *executed-results* (make-hash-table :test #'equal))
(setf (gethash "subtotal" *executed-results*) 42)

(defun lookup-babel-result (block-name variables)
  (declare (ignore variables))
  (multiple-value-bind (value found-p)
      (gethash block-name *executed-results*)
    (unless found-p
      (error "No result for Babel block ~A" block-name))
    value))

(org-kit:resolve-babel-variables
 *babel-document* *consumer-block*
 :result-resolver #'lookup-babel-result)
```

For the document above, this returns `(("total" . 42))`.

In the example, `#+NAME: subtotal-call` labels `#+CALL: subtotal()` for `:var total=subtotal-call`.
The callback receives `subtotal` as the source-block name.
Without a callback, an executable result reference signals `org-kit:org-babel-error`.
Its reason is `:missing-result-resolver`.
The `:max-babel-result-elements` limit bounds callback results resolved by both
`org-kit:resolve-babel-variables` and `org-kit:expand-babel-noweb`.
It defaults to 1,000,000 visited objects per callback result; containers and each list cell count as objects.
Pass `:limits (org-kit:make-limits :max-babel-result-elements N)` to `org-kit:parse-org` to set a different value.

`#+NAME: name` or `#+RESULTS: name` labels a static Org pipe table or plain list in the same document for `:var` lookup without a callback; use `name` as the `:var` value.
A table's rule rows are omitted.
Other named node types, such as paragraphs and headlines, and tables formatted by Emacs's `table.el` package rather than as Org pipe tables signal `org-kit:org-babel-error`.
List items with nested content, multiple children, or a non-paragraph child signal `org-kit:org-babel-error`.

## Noweb expansion

Pass `org-kit:expand-babel-noweb` a document, one of its source blocks, and a context: `:eval`, `:tangle`, or `:export`.
Use `:eval` when preparing code for evaluation, `:tangle` when preparing code to tangle to a file, and `:export` when exporting.
It returns the block body without its final line ending.
`:noweb` selects when references expand:

| `:noweb` value | `:eval` | `:tangle` | `:export` |
|---|---|---|---|
| `yes` | Expand | Expand | Expand |
| `no` | Keep references | Keep references | Keep references |
| `eval` | Expand | Keep references | Keep references |
| `tangle` | Keep references | Expand | Keep references |
| `strip-export` | Expand | Expand | Remove reference markers |
| `no-export` | Expand | Expand | Keep references |
| `strip-tangle` | Expand | Remove reference markers | Expand |

For `strip-export` and `strip-tangle`, the reference marker is removed and the referenced body is not inserted.

For `<<name>>`, a source block named `name` takes precedence and its body is inserted.
If there is no such block, `<<name>>` expands the group of blocks with `:noweb-ref name` in document order.
Before each group member after the first, expansion inserts `:noweb-sep` from the previous member.
The default separator is a newline.
An unknown plain reference expands to an empty string.
Referenced blocks expand recursively only when their own `:noweb` mode allows
expansion in that context. Otherwise their body is inserted with any noweb
markers it contains left unchanged.

`<<name()>>` requests a block result through `:result-resolver`.
Pass the same callback used by `org-kit:resolve-babel-variables` with the `:result-resolver` keyword to `org-kit:expand-babel-noweb`:

```lisp
(org-kit:expand-babel-noweb
 *babel-document* *consumer-block* :context :eval
 :result-resolver #'lookup-babel-result)
```

With the document above, this returns `(print total)` followed by `42`.

String results are inserted as written.
Numbers, characters, symbols, conses, and vectors use Common Lisp printed form.

By default, all text before `<<name>>` on its source line prefixes the first inserted line and repeats on later inserted lines.
For example, `prefix: <<name>>` with `first` and `second` on separate lines becomes `prefix: first` and `prefix: second`.
Use `:noweb-prefix no` to disable this behavior.
Cycles signal `org-kit:org-babel-error`.
Depth and cumulative expanded-character ceilings signal `org-kit:org-resource-limit-error`.

See [Resource limits](../reference/node-model.md#resource-limits) for the Babel ceilings.
See [API stability](../reference/api-stability.md) for the public API policy.

## Executing source blocks and calls

`org-kit:register-babel-executor` registers an executor in a caller-owned
`org-kit:callback-registry` under the `:babel-executor` kind and a language
name. No executor is registered by default. The executor receives the expanded
body, the resolved header-parameter alist, and the resolved variable alist.
Parameter keys are uppercase strings, and `"SESSION"` carries the session
identifier. The executor owns any state associated with that identifier.

```lisp
(defparameter *babel-executors* (org-kit:make-callback-registry))

(org-kit:register-babel-executor
 *babel-executors* "emacs-lisp"
 (lambda (body parameters variables)
   (declare (ignore parameters variables))
   body))

(org-kit:execute-babel-source-block
 *babel-document* *subtotal-block* *babel-executors*)
```

An executor for a real language would run or otherwise process `body` according
to the caller's policy.

`org-kit:make-babel-common-lisp-reference-executor` returns an opt-in executor
that evaluates source-block code in the current Lisp process. Register it only
for trusted documents. A custom executor may also evaluate or pass block bodies
to another program; its caller decides which documents it may process.

`org-kit:execute-babel-source-block` runs a source block through its registered
executor and returns an `org-kit:babel-execution-result`. Its status is
`:executed`, `:cached`, `:skipped`, or `:cancelled`. Executed results include
the raw value, the cache hash when enabled, and an `edit-result` for the
`#+RESULTS` update. Read the updated document through that edit result with
`org-kit:edit-result-updated-document`; the input document object is unchanged.
A skipped, cancelled, or cached result leaves the document unchanged.

Pass `:evaluation-context :export` when preparing an export. `:eval no` and
`:eval never` skip execution; `:eval no-export` skips only in export context.
For `:eval query`, pass a `:confirmation-function` that receives the block and
its effective parameters. A missing confirmation function signals
`org-kit:org-babel-error` with reason `:confirmation-required`. A non-nil return
value allows execution; `nil` returns a `:cancelled` result.

For `:cache yes`, pass a `:hash-function` that accepts the expanded body,
effective parameters, and resolved variable values, and returns a string. A
matching hash on the adjacent `#+RESULTS[hash]` line skips execution. The hash
identifies inputs for cache invalidation; it is not an integrity check. Inline
calls have no persistent `#+RESULTS` line, so they run on each request.

Pass `:result-resolver` to source-block or call execution when the block uses
executable `:var` references or `<<name()>>` noweb references. The resolver
receives the referenced block name and resolved variable alist, and returns a
Common Lisp value. The executor still receives the expanded body and resolved
variables; the library does not execute the referenced block for the resolver.

`org-kit:execute-babel-call` accepts a `#+CALL` or inline-call node. It resolves
named blocks in the current document first, then searches the supplied
`:library-documents` in order. Those documents are parsed by the caller; the
library does not load files. Call arguments and call-site headers override the
target block's matching variable and parameter values. The call node describes
the invocation; execution uses the resolved target source block and its body.

If the `subtotal` block declares `:var value=0`, then
`#+CALL: subtotal(value=4)` passes `4` as that variable's value. Parse the call
line and pass its node to `org-kit:execute-babel-call`.

For example, `*subtotal-call*` is the parsed `#+CALL: subtotal()` node from the
document fixture. Passing it with the same registry executes the named
`subtotal` block:

```lisp
(org-kit:execute-babel-call
 *babel-document* *subtotal-call* *babel-executors*)
```

`org-kit:update-babel-results` formats a supplied raw value and returns an
`edit-result`. It does not change the document object. The `output` mode accepts
only string values. Result formatting supports the requested `value` and
`output` modes, `table`, `list`, `verbatim`,
`raw`, `drawer`, `org`, `html`, and `code` formats, and `replace`, `append`,
`prepend`, and `silent` operations. Unsupported result instructions signal
`org-kit:org-babel-error` instead of being ignored.

The Common Lisp reference executor returns the final form's value for `value`
results and captured standard, error, and trace output for `output` results.
Custom executors receive the resolved `RESULTS` parameter and decide how to
produce that raw value.

Its required arguments are a document, a source-block or `#+CALL` node from
that document, and the raw result value:

```lisp
(org-kit:update-babel-results
 *babel-document* *subtotal-call* "42")
```

## Tangling

`org-kit:babel-tangle` returns an `org-kit:write-plan`. It groups blocks by
their `:tangle` path in document order and applies `:mkdirp`, `:comments`,
`:shebang`, `:padline`, and `:tangle-mode`. `:tangle yes` derives the output
name from the document source name and block language. `:target-file` supplies
the default path when `:tangle` is absent from generic or language-specific
properties, heading properties, and the source block header or switches. An
explicit `:tangle` value at any of those levels, including `:tangle no`, takes
precedence. `:language-regexp` filters blocks by language. Every plan
path is relative to the root later passed to `org-kit:apply-write-plan`; an
absolute path or parent component signals `org-kit:org-write-plan-error` while
the plan is built.

```lisp
(defparameter *tangle-document*
  (org-kit:parse-org
   (format nil
           "#+BEGIN_SRC emacs-lisp :tangle yes~%42~%#+END_SRC~%#+BEGIN_SRC emacs-lisp~%43~%#+END_SRC~%")
   :source-name "example.org"))

(defparameter *tangle-plan*
  (org-kit:babel-tangle *tangle-document* :target-file "fallback.el"))

(org-kit:write-plan-entry-path
 (aref (org-kit:write-plan-entries *tangle-plan*) 0))
;; => "example.el"

(org-kit:write-plan-entry-path
 (aref (org-kit:write-plan-entries *tangle-plan*) 1))
;; => "fallback.el"

;; Apply only after choosing a destination root.
(org-kit:apply-write-plan *tangle-plan* *destination-root*)
```

Tangling never writes files. It uses `org-kit:expand-babel-noweb` with the
`:tangle` context. `:comment-renderer` receives `(block kind target-path)`,
where `kind` is `:link` or `:org`. For `:link`, it returns the before-body and
after-body strings as two values. For `:org`, it returns the before-body string.
`:comments no` requests no comment renderer. In this library, `yes`, `link`, and
`noweb` all request the same `:link` renderer call; `org` requests `:org`; and
`both` requests `:org` followed by `:link`.
The library has no built-in comment renderer, so every requested comment mode
requires `:comment-renderer`. `:variable-renderer` receives
`(block resolved-variable-alist)` and is required for blocks with `:var`.
Pass `:result-resolver` to `babel-tangle` to resolve executable `:var` values
for that renderer and `<<name()>>` references in tangled bodies. It returns
values for the caller; it does not execute a referenced source block.
For example, a link renderer can return `"# [[file:source.org][block]]"` and
`"# block ends here"`; a variable renderer for `answer=42` can return
`"answer=42"`.
Tangle content and result formatting enforce the document's Babel and
write-plan limits before constructing output strings.

## Detangling

`org-kit:babel-detangle` takes a parsed document and tangling output text. It
returns an `org-kit:edit-result` with the updated document and source spans. It
does not read or write files or start a process.

Org's tangle comments pair a link such as `[[file:source.org::*Heading][Heading:1]]`
with an end marker such as `Heading:1 ends here`. The comment prefix depends on
the language, so detangling recognizes the Org link and end-marker text within
the rendered comment lines without requiring a particular prefix. A named
source block uses its `#+NAME` value. An unnamed block uses `Heading:n`, where
`n` is its source-block position within that heading, or `No heading:n` when it
is outside a heading. Named blocks count toward that position.

The source name is the link's display text, between `][` and `]]`; the file
target is not opened. That name resolves only against blocks in the supplied
document. A link whose display text does not match a source block is ignored,
and unrelated text is unchanged. A recognized start link must have a matching
`<source-name> ends here` line before another linked section starts; a missing
or mismatched end marker signals an error.
Detangling replaces only matched source-block bodies; text before, between, and
after those bodies remains byte-for-byte unchanged. Duplicate source names,
including collisions between a named block and a generated label, signal
`org-kit:org-babel-error`. `max-input-length` limits both strings, while
`max-search-work` bounds the cumulative marker scan. The resulting edit
enforces output character and octet limits. See [Resource limits](../reference/node-model.md#resource-limits)
for their defaults and the other Babel ceilings.

```lisp
(defparameter *detangle-document*
  (org-kit:parse-org
   (format nil "#+NAME: sample~%#+BEGIN_SRC text~%old~%#+END_SRC~%")))

(defparameter *detangle-result*
  (org-kit:babel-detangle
   *detangle-document*
   (format nil "# [[file:example.org][sample]]~%new~%# sample ends here")))

(org-kit:document-text
 (org-kit:edit-result-updated-document *detangle-result*))
```

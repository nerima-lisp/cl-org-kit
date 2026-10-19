# Babel headers and noweb

The Babel APIs resolve source block parameters and expand noweb references from an Org document.
They do not execute source blocks; callers supply executable results through a result resolver.

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
```

## Header arguments

Pass `org-kit:resolve-babel-parameters` a document and one of its `:src-block` nodes.
It returns an alist of key-value pairs.
Keys are uppercase strings.
Values are strings, or `nil` for flags.

Resolution applies defaults, then generic `header-args` properties, language-specific properties, affiliated `#+HEADER` arguments, and source block arguments.
A later value replaces an earlier value.
The built-in values are `:session none`, `:results replace`, `:exports code`, `:cache no`, `:noweb no`, `:hlines no`, and `:tangle no`.
Headings inherit values within each property layer, so an inherited language-specific value can override an inherited generic value.
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
#+BEGIN_SRC emacs-lisp
body
#+END_SRC
```

The source block resolves `SESSION` to `parent-language` and includes the `child=1` variable.

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
Referenced blocks expand recursively only when their own `:noweb` mode allows expansion in that context.

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

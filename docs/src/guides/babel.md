# Babel headers and noweb

The Babel APIs resolve source block parameters and expand noweb references from an Org document.
They do not execute source blocks; callers supply executable results through a result resolver.

## Header arguments

Pass `resolve-babel-parameters` a document and one of its `:src-block` nodes.
Find source blocks by walking the parsed root with `map-nodes`; see [Node Model](../reference/node-model.md#nodes).
It returns an ordered alist.
Keys are uppercase strings.
Values are strings, or `nil` for flags.

Resolution applies defaults, then generic `header-args` properties.
It applies language-specific properties next.
Heading inheritance is resolved within each property layer, so a language-specific value can override an inherited generic value.
Affiliated `#+HEADER` arguments follow the property layers.
Source block arguments have the highest priority.
A later value replaces an earlier value.
`:var` assignments merge by variable name.
Mutually exclusive `:results` values replace earlier members of their group.
The groups cover format (`file`, `list`, `vector`, `table`, `scalar`, `verbatim`), representation (`raw`, `html`, `latex`, `org`, `code`, `pp`, `drawer`, `link`, `graphics`), handling (`replace`, `silent`, `none`, `discard`, `append`, `prepend`), and `output` versus `value`.
One value from each group can be combined in a header; a later value replaces only an earlier value from its own group.
For `:exports`, `code`, `results`, `both`, and `none` are mutually exclusive.

```lisp
(org-kit:resolve-babel-parameters document src-block)
```

The call returns raw header values.
Use `resolve-babel-variables` to parse the block's `:var` assignments.

## Static `:var` values

`resolve-babel-variables` returns an ordered alist from variable names to Common Lisp values.
It accepts numeric literals, double-quoted strings, named Org pipe tables, and simple named lists.
Numeric table cells become numbers.
Other unquoted table cells and list items remain strings.
A named plain list can be used as a `:var` value when each item contains one paragraph without nested content.

```org
#+NAME: sample-items
- red
- blue

#+NAME: consumer
#+BEGIN_SRC text :var items=sample-items
#+END_SRC
```

For a `:var` assignment, a double-quoted value is a string and a numeric value such as `7` is parsed as a number.
A bare value such as `sample-items` names another Org object; a missing name signals `org-babel-error`.
Reader syntax such as `(+ 1 2)` is rejected instead of being evaluated.
The library never evaluates a `:var` expression or source block.
A named source block or `#+CALL` result requires the `:result-resolver` function.
The callback receives `(block-name resolved-variable-alist)` and returns that block's result as a Common Lisp value.
For example, a host can look up values it computed earlier by block name:

```lisp
(defparameter *executed-results* (make-hash-table :test #'equal))
(setf (gethash "subtotal" *executed-results*) 42)

(defun lookup-babel-result (block-name variables)
  (declare (ignore variables))
  (gethash block-name *executed-results*))

(org-kit:resolve-babel-variables
 document src-block :result-resolver #'lookup-babel-result)
```

A `#+CALL` result resolves through its named source block and passes that block's name to the callback.
Without a callback, an executable result reference signals `org-babel-error`.
Its reason is `:missing-result-resolver`.

`#+NAME` can name an Org pipe table or a simple list for local resolution.
`#+RESULTS: name` can name a static result table or list in the same way.
A table's rule rows are omitted.
Other named node types, such as paragraphs and headlines, and tables formatted by Emacs's `table.el` package rather than as Org pipe tables signal `org-babel-error`.
Nested lists and multi-part list items signal the same condition.

## Noweb expansion

Pass `expand-babel-noweb` a source block and one context: `:eval`, `:tangle`, or `:export`.
Use `:eval` when preparing code for evaluation, `:tangle` when preparing code to tangle to a file, and `:export` when exporting.
It returns the block body without its final line ending.
When `:noweb` is omitted, its mode defaults to `no`.
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

`<<name>>` inserts a named source block's body.
A block with `:noweb-ref name` contributes to its group in document order.
Each group member's `:noweb-sep` separates it from the next member.
The default separator is a newline.
An unknown plain reference expands to an empty string.
Referenced blocks expand recursively only when their own `:noweb` mode allows expansion in that context.

`<<name()>>` requests a block result through `:result-resolver`.
Pass the same callback used by `resolve-babel-variables` with the `:result-resolver` keyword to `expand-babel-noweb`:

```lisp
(org-kit:expand-babel-noweb
 document src-block :context :eval
 :result-resolver #'lookup-babel-result)
```

String results are inserted as written.
Numbers, characters, symbols, conses, and vectors use Common Lisp printed form; `NIL` prints as `NIL`.
A missing callback signals `org-babel-error`.

By default, text before `<<name>>` on its source line remains before the first inserted line and is repeated at the start of each later inserted line. For example, `  <<name>>` with a two-line body `first\nsecond` becomes `  first\n  second`.
Use `:noweb-prefix no` to disable this behavior.
Cycles signal `org-babel-error`.
Depth and cumulative expanded-character ceilings signal `org-resource-limit-error`.

See [Resource limits](../reference/node-model.md#resource-limits) for the Babel ceilings.
See [API stability](../reference/api-stability.md) for the public API policy.

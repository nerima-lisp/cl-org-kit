# Callbacks and Write Plans

`cl-org-kit` does not include an Emacs Lisp evaluator or run external programs. A caller-owned callback registry supplies those functions explicitly. File output uses a separate write plan, so callers can inspect its entries before applying them.

## Register callbacks explicitly

Create a registry and pass it to the operation that needs a callback. A registry has no callbacks until the caller adds them.

```lisp
(let ((registry (org-kit:make-callback-registry)))
  (org-kit:register-callback
   registry :evaluator :emacs-lisp
   (lambda (expression)
     (my-elisp-evaluator expression)))
  (org-kit:evaluate-elisp registry "(+ 1 2)"))
```

`evaluate-elisp` passes its expression or form unchanged to the registered function. The library does not parse or evaluate it. If the registry has no `:evaluator` callback named `:emacs-lisp`, the call signals `org-elisp-evaluator-error`. Conditions signalled by a registered function propagate unchanged.

The same registry supports later features that need named functions. For example, a Babel executor can use kind `:babel-executor` and a language name such as `"python"`; a crypt implementation can use kind `:crypt-encrypt` and a name such as `:org-crypt`. `register-callback` folds kinds to uppercase and names to lowercase. Registering the same kind and name again replaces the earlier function.

## Build and inspect a write plan

`make-write-plan-entry` and `make-write-plan` create data only. Entry paths are relative POSIX paths. Content may be a string, which is encoded as UTF-8, or an octet vector, which is written unchanged. The default file mode is `#o644`. Set `:create-directories-p t` when the apply operation may create missing parent directories.

```lisp
(let* ((entry
         (org-kit:make-write-plan-entry
          "generated/report.org" (format nil "* Report~%")
          :mode #o600
          :create-directories-p t))
       (plan (org-kit:make-write-plan (list entry))))
  (org-kit:write-plan-entries plan))
```

The returned entry vector contains the plan's entries. Use `write-plan-entry-path` and `write-plan-entry-content` to inspect each path and its content before applying the plan:

```lisp
(org-kit:apply-write-plan plan output-root)
```

`output-root` must name an existing directory. The result is a list of normalized relative paths in plan order. Existing regular files may be replaced. A directory target or another special file is rejected.

## Validation and limits

Before creating directories or files, `apply-write-plan` checks every path, duplicate, target type, entry count, and total content size. It rejects absolute paths, any `..` component, duplicate normalized paths, file-and-parent conflicts, and existing symlinks in every path component, including the target. Paths that differ only by letter case are also treated as duplicates, which avoids aliases on case-insensitive filesystems. A plan rejected during this initial full preflight creates no directories or target files.

`make-limits` accepts `:max-write-plan-entries` and `:max-write-plan-bytes`. The byte limit counts UTF-8-encoded string octets and exact octet-vector lengths across all entries. Exceeding either limit signals `org-resource-limit-error` before filesystem changes.

For each entry, application rechecks path components immediately before creating directories, opening a temporary file, and renaming it to the target. The temporary file is created with `O_CREAT`, `O_EXCL`, and `O_NOFOLLOW`; its mode is set before rename. Rename replaces each target atomically on the same filesystem. A plan with multiple files is not transactional: an I/O failure or a revalidation failure on a later entry can leave earlier entries applied.

Path checks use names rather than directory file descriptors. A process that can modify a parent directory can replace it between revalidation and the next filesystem operation. That race remains outside this API's guarantee. Treat the resolved `output-root` directory as a trusted boundary and prevent concurrent changes to it and to the parent directories named by the plan while applying it. Filesystem-specific aliases caused by Unicode normalization are outside the lexical duplicate check.

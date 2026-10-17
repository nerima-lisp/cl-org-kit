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

The same registry can supply named functions to other features. For example, a caller can register a
Babel executor under kind `:babel-executor` and name `"python"`, then call it with
`(org-kit:invoke-callback registry :babel-executor "python" source)`. A crypt callback could use kind
`:crypt-encrypt` and name `:org-crypt`. `register-callback` folds kinds to uppercase and names to
lowercase. Registering the same kind and name again replaces the earlier function. Registry
operations are not thread-safe. Callers must synchronize access when multiple threads register or
invoke callbacks through the same registry.

## Build and inspect a write plan

`make-write-plan-entry` and `make-write-plan` create data only. Entry paths are relative POSIX paths. Content may be a string, which is encoded as UTF-8, or an octet vector, which is written unchanged. The default file mode is `#o644`. Set `:create-directories-p t` when the apply operation may create missing parent directories.

```lisp
(defun write-report (output-root)
  (let* ((entry
           (org-kit:make-write-plan-entry
            "generated/report.org" (format nil "* Report~%")
            :mode #o600
            :create-directories-p t))
         (plan (org-kit:make-write-plan (list entry))))
    (format t "Output root: ~S~%Entries: ~S~%"
            output-root
            (loop for planned-entry across (org-kit:write-plan-entries plan)
                  collect (list :path
                                (org-kit:write-plan-entry-path planned-entry)
                                :content
                                (org-kit:write-plan-entry-content planned-entry)
                                :mode
                                (org-kit:write-plan-entry-mode planned-entry)
                                :create-directories-p
                                (org-kit:write-plan-entry-create-directories-p
                                 planned-entry))))
    (when (y-or-n-p "Apply this write plan? ")
      (org-kit:apply-write-plan plan output-root))))
```

`output-root` must name an existing directory. The returned entry vector is a
copy of the plan's entries, and entry inspection returns each path as stored.
Applying the plan returns normalized paths: for example, `a/./b.txt` becomes
`a/b.txt`. Entries with the same normalized path are duplicates.

The library resolves `output-root` with `truename`, so a symlink used as
`output-root` resolves to its target. Applying the plan returns normalized
relative paths in plan order. Existing regular files may be replaced. A
directory target or another special file is rejected.

## Validation and limits

Before creating directories or files, `apply-write-plan` checks every path, duplicate paths,
target types, entry count, and total content size. It rejects absolute paths, any `..` component,
duplicate normalized paths, file-and-parent conflicts, and existing symlinks in each path component
below `output-root`, including the target. Paths that differ only by letter case are also treated as
duplicates, which reduces aliases on case-insensitive filesystems. A plan rejected during this
initial full preflight creates no directories or target files.

`make-limits` accepts `:max-write-plan-entries` and `:max-write-plan-bytes`. The byte limit counts UTF-8-encoded string octets and exact octet-vector lengths across all entries. Exceeding either limit signals `org-resource-limit-error` before filesystem changes.

During application, the library opens the resolved output root and walks each parent component
with `openat` using `O_DIRECTORY | O_NOFOLLOW`. It creates missing parent directories with
`mkdirat`, then keeps the final parent descriptor while it creates a same-directory temporary file
with `openat` using `O_CREAT`, `O_EXCL`, and `O_NOFOLLOW`. It sets the file mode, calls `fsync`, and
uses `renameat` relative to that parent descriptor to replace the target atomically on the same
filesystem. Filesystem operation failures usually signal `org-write-plan-error` with reason
`:io-failed`; `org-kit:org-write-plan-error-cause` returns the underlying condition when available.
If a parent disappears after preflight and directory creation is disabled, the reason is
`:missing-parent-directory`. A plan with multiple files is not transactional: a failure while
applying a later entry can leave earlier entries applied.

The final parent descriptor keeps file creation and rename attached to the
directory opened for that entry, even if an ancestor pathname later changes.
The API does not lock directory entries while walking the path, so callers must
prevent concurrent renames or replacements of path components during
application. The lexical duplicate check does not account for filesystem
Unicode normalization, so a filesystem may treat distinct Unicode spellings
of a name as aliases.

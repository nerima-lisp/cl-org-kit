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

`evaluate-elisp` passes EXPRESSION unchanged to the registered function; the library does not parse or evaluate it. If the registry has no `:evaluator` callback named `:emacs-lisp`, the call signals `org-elisp-evaluator-error`. Conditions signalled by a registered function propagate unchanged.

`evaluate-elisp` also accepts an optional positional context. When supplied, the
registered evaluator receives EXPRESSION and CONTEXT as its two arguments;
without it, the evaluator is still called with one argument. Diary evaluation
from `agenda` and structured `query-list` calls supplies a plist with `:entry`
and `:date` keys. `:entry` is the diary entry text (an empty string for a diary
timestamp), and `:date` is a Gregorian `(month day year)` list. The expression
is the complete parenthesized Emacs Lisp form, with the Org `%%` marker and any
trailing diary text removed. When no evaluator is registered, diary evaluation
signals `org-elisp-evaluator-error` with reason `:unregistered`.

A callback registered as an executor reports a timeout by signalling `org-callback-timeout-error`. The condition provides `org-callback-error-kind`, `org-callback-error-name`, and `org-callback-error-reason`; the reason defaults to `:timeout`. `invoke-callback` propagates the condition unchanged. The executor enforces its own deadline. cl-org-kit does not start a process or thread or interrupt the callback to enforce it.

The same registry can supply named functions to other features. For example, a caller can register a
Babel executor under kind `:babel-executor` and name `"python"`, then call it with
`(org-kit:invoke-callback registry :babel-executor "python" source)`. `register-callback` folds kinds
to uppercase and names to lowercase. Registering the same kind and name again replaces the earlier
function. Registry operations are not thread-safe. Callers must synchronize access when multiple
threads register or invoke callbacks through the same registry.

## Build and inspect a write plan

`make-write-plan-entry` and `make-write-plan` create data without filesystem
changes. Entry paths are relative POSIX paths. The library encodes string
content as UTF-8 and writes octet vectors unchanged.

Use `make-write-plan-delete-entry` to add a file deletion to a plan.
`write-plan-entry-operation` returns `:write` or `:delete` when inspecting an
entry. A delete entry has no content, and applying it requires an existing
regular file below the output root. Deletions share the same path validation,
conflict checks, and resource limits as writes. A mixed plan is applied in
entry order and is not transactional; earlier operations may remain applied if
a later operation fails.

`write-plan-entry-mode` reports `#o644` when `:mode` is omitted. Applying an
entry also tracks whether the caller supplied `:mode`. If `:mode` is omitted,
apply preserves an existing regular file's mode and uses `#o644` for a new file.
If `:mode` is supplied, apply sets that mode exactly, regardless of the process
umask.

Set `:create-directories-p t` when applying the plan may need to create missing
parent directories.

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

`make-limits` accepts `:max-write-plan-entries`, `:max-write-plan-bytes`,
`:max-write-plan-path-bytes`, and `:max-write-plan-path-depth`. The content-byte
limit counts UTF-8 string octets and exact octet-vector lengths across all
entries. The path-byte limit counts UTF-8 bytes in each entry path. The
path-depth limit counts non-empty components other than `.`. Both path limits
are checked before normalization. Their defaults are 4096 bytes and 128
components. Exceeding any limit signals `org-resource-limit-error` before
filesystem changes.

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
directory opened for that entry. After each write, the library opens the parent
path again from the root descriptor and compares its device and inode with the
descriptor used for the write. If they differ or the path cannot be reopened,
it signals `org-write-plan-error` with reason `:containment-lost` and stops
before applying later entries.

The check catches a missing or different parent at the re-open. A path restored
before that check is not detected, and a rename can still happen during a write
or after the check. Callers must ensure that no other actor can rename or
replace path components while the plan is applied. The library rejects absolute
paths, `..`, NUL, and symlink components; those checks
do not remove the concurrent-rename trust boundary. A concurrent rename can
move an already-written file.

Preflight conflict keys are case-folded and normalized to NFC, so canonically
equivalent spellings of a path are treated as duplicates. Filesystem operations
retain each entry's original path spelling.

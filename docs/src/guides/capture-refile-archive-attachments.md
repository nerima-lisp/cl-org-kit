# Capture, Refile, Archive, and Attachments

These APIs operate on parsed Org documents and caller-provided values. The built-in capture, archive, and attachment-plan functions do not prompt or access files. A caller-supplied evaluator for `%(...)` runs during template expansion and may have its own side effects. `apply-write-plan` is the filesystem boundary for attachment plans: it applies the write or delete entries. Template expansion returns a capture-template result; capture insertion returns a capture-target result containing an edit result, the updated document, and a cursor offset. Archive edits return edit results, and attachment builders return plans for inspection and application.

Create reusable sample inputs for the snippets:

```lisp
(defun first-headline (document)
  (let ((found nil))
    (org-kit:map-nodes
     (lambda (node)
       (when (and (null found) (eq (org-kit:node-type node) :headline))
         (setf found node)))
     (org-kit:document-root document))
    found))

(defparameter *time-context*
  (org-kit:make-time-context
   :reference-time (org-kit:make-org-time-value "2025-01-01T10:30:00")
   :zone (org-kit:make-fixed-time-zone 0)))
(defparameter *document*
  (org-kit:parse-org
   (format nil "* Inbox~%:PROPERTIES:~%:ID: example-inbox~%:END:~%")
   :source-name "notes.org"))
(defparameter *heading* (first-headline *document*))
(defparameter *archive-document*
  (org-kit:parse-org (format nil "* References :reference:~%")
                     :source-name "archive.org"))
(defparameter *attachment-bytes*
  (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)))
```

## Expand and insert a capture

Pass a time context and supply every value that a template requests. The expander supports `%?`, `%t`, `%T`, `%u`, `%U`, `%i`, `%a`, `%^{prompt}`, `%^g`, `%^t`, `%(expression)`, and `%[file]`. Pass the string values for `%i` and `%a` through `:initial` and `:annotation`; `:prompt-answers` maps prompt names to strings.

`%^g` takes `:tag-answer` as a string or list of strings. `%^t` takes `:prompted-time` as an Org time string, Org time value, local date, or local date-time. For `%[file]`, pass an alist of path strings and string contents through `expand-capture-template`'s `:file-contents`. The first `%?` becomes the returned cursor offset; if there is no `%?`, the offset is 0. An Emacs Lisp placeholder calls the evaluator in `:callbacks` and accepts a string or `NIL` result.

Register the evaluator on a caller-owned callback registry:

```lisp
(let ((callbacks (org-kit:make-callback-registry)))
  (org-kit:register-callback
   callbacks :evaluator :emacs-lisp
   (lambda (expression)
     (declare (ignore expression))
     "Evaluated text"))
  (org-kit:expand-capture-template
   "%(expression)" :time-context *time-context* :callbacks callbacks))
```

```lisp
(let* ((template
         (org-kit:expand-capture-template
          (format nil "** TODO %^{Title} %?~%%t~%%a")
          :time-context *time-context*
          :prompt-answers '(("Title" . "Review"))
          :annotation "[[file:notes.org::*Inbox][Inbox]]"))
       (capture
         (org-kit:capture-into-target
          (list *document*)
          '(:file+headline "notes.org" "Inbox")
          template
          :time-context *time-context*)))
  (values (org-kit:edit-result-updated-document
           (org-kit:capture-target-result-edit-result capture))
          (org-kit:capture-target-result-cursor-offset capture)))
```

The cursor offset is absolute in the updated target document. Target-spec shapes are:

- `(:file+headline "notes.org" "Inbox")`
- `(:file+olp "notes.org" ("Work" "Projects"))`
- `(:file+olp+datetree "notes.org")`
- `(:id "headline-id")`
- `(:file+regexp "notes.org" "^\\* Inbox$")`

The headline, outline, and datetree modes create missing headings. Datetree creates year, month, and day headings from the supplied time context. Pass `:time-context` to `capture-into-target` for every target mode; it is required even when the target is not a datetree. An ID target must resolve to exactly one headline among the supplied documents. If the target file is not already among `documents`, pass an alist of target filenames and their original Org text through `capture-into-target`'s `:file-contents`. The updated document is returned in the result without writing it.

## List refile candidates

`refile-target-candidates` takes a current document and a list of two-element specifications: a document or document list (or `NIL` for the current document), followed by a selector. Selectors are `T`, `(:level . n)`, `(:maxlevel . n)`, `(:tag . string)`, and `(:todo . string)`. An empty specification list selects top-level headings from the current document. Results preserve traversal order and deduplicate headings by node identity. Tag selection uses local tags.

```lisp
(org-kit:refile-target-candidates
 *document*
 (list (list nil '(:maxlevel . 2))
       (list (list *archive-document*) '(:tag . "reference"))))
```

The function enumerates candidates only. It does not resolve filenames, display a chooser, or move a subtree.

## Resolve archive destinations and archive a sibling

`resolve-archive-location` returns a cons of archive file and heading. It checks the heading's `ARCHIVE` property, then inherited `ARCHIVE` properties on its ancestors, then document `#+ARCHIVE`, then `:default-location`, and finally `%s_archive::`. `%s` expands to the source file's basename. When a source name is available, relative archive files are resolved against its directory; without one, a relative archive path is returned as supplied. The built-in `%s_archive::` fallback requires a document source name. The trailing `::` separates the file from an empty archive-heading string. Pass `:default-location` when the caller wants a different fallback.

`archive-to-sibling` requires an explicit `:time-context`, records `ARCHIVE_TIME` from it, and moves the heading below an `Archive` sibling tagged `ARCHIVE` in the same document. It returns an edit result and performs no file I/O. The sibling name and tag use Org's defaults and are not configurable through this API.

## Build attachment write plans

`resolve-attachment-directory` returns a relative path. It checks `DIR` before `ATTACH_DIR`, including inherited values when inheritance is enabled; an inherited `DIR` can therefore take precedence over a local `ATTACH_DIR`. If neither is present, it uses the heading's `ID` under `:attachment-root` (default `"data"`). IDs longer than two characters use a two-character prefix directory followed by the remaining ID. For example, `AB12` maps to `data/AB/12`, and `AB` maps to `data/__/A/AB`. `:source-directory` supplies the base for absolute `DIR` or `ATTACH_DIR` paths; otherwise the directory containing the document source path is used. Absolute property paths require a usable absolute base and must stay within it; paths outside the base are rejected. Inherited attachment properties are considered only when an ancestor enables `ATTACH_DIR_INHERIT` with a non-empty value other than `nil`, `no`, or `false` (case-insensitive).

`add-attachment` returns a write plan with one write entry. `delete-attachment` returns a write plan with one delete entry. Both validate the filename and path against the document limits. A write entry replaces an existing target file; a delete entry requires its target to exist. The plan's relative path is resolved below the output root you pass to `apply-write-plan`.

```lisp
(defun add-report-attachment (existing-output-directory)
  (let ((plan (org-kit:add-attachment
               *document* *heading* "report.pdf" *attachment-bytes*
               :attachment-root "attachments")))
    (org-kit:apply-write-plan plan existing-output-directory)))
```

The function argument `existing-output-directory` must name an existing directory. A plan containing multiple entries is not transactional.

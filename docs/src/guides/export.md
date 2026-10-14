# Export

`export-org` renders a parsed document as HTML or GitHub Flavored Markdown
(GFM). The default output type is a string.

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
   (org-kit:export-org document :gfm
                       :context context :output-type :octets)))
~~~

The first two calls return strings. `:output-type :octets` returns a UTF-8
octet vector. With `:output-type :stream`, supply an output stream with
`:stream`. Character streams receive text; streams whose element type is
`(unsigned-byte 8)` receive UTF-8 octets. Other stream element types are
rejected, and a successful call returns the stream.

`make-export-context` supplies optional title, author, date, language, and time
context values. Non-NIL context metadata overrides the corresponding Org
keywords such as `#+TITLE`; NIL leaves the in-document value in effect.

ASCII export is not available in version 0.1.0.

## Errors and limits

Unsupported syntax for a backend signals `org-export-unsupported-syntax` with
the backend, source excerpt, position, and feature. Other export failures,
including an invalid backend or output configuration, signal
`org-export-error`. Invalid context values can signal `type-error`.

The document limits cap output at 64 Mi characters or 256 Mi UTF-8 octets.
Export expansion is limited to depth 32, 64 Mi expanded characters, and 4 Mi
expanded nodes by default. Override applicable ceilings with `make-limits` when
parsing the document. Exceeding a configured ceiling signals
`org-resource-limit-error`.

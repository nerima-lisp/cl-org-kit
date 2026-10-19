# Org crypt

`cl-org-kit` edits Org crypt payloads through callbacks supplied by the caller. It does not start GPG or another process. The callback decides how plaintext is encrypted and how ciphertext is decrypted.

For instructions on adding a checkout and its dependencies to the ASDF source registry, see [Getting Started](../getting-started.md). Once `cl-org-kit` is available through your ASDF source registry, load it before evaluating the examples:

```lisp
(require :asdf)
(asdf:load-system "cl-org-kit")
```

## Register callbacks

Create a callback registry and register one function for encryption and one for decryption. Each function receives one string and returns a string. The encryption callback returns one complete PGP MESSAGE armor block; the decryption callback returns plaintext. Replace `my-encrypt-function` and `my-decrypt-function` in the examples with your application functions. This form returns the configured registry for the edit calls below.

```lisp
(let ((registry (org-kit:make-callback-registry)))
  (org-kit:register-callback
   registry :crypt-encrypt "org-crypt"
   (lambda (plaintext)
     (my-encrypt-function plaintext)))
  (org-kit:register-callback
   registry :crypt-decrypt "org-crypt"
   (lambda (armor)
     (my-decrypt-function armor)))
  registry)
```

The callback name is always `"org-crypt"`. Callers own the registry. A missing callback signals `org-crypt-error` with reason `:missing-callback`. An error signaled by a callback is wrapped in the same condition with reason `:callback-failed`. A non-string result or malformed encryption armor uses reason `:invalid-callback-result`. Callback errors are not retained: `org-crypt-error-cause` returns `nil` so it cannot expose callback data such as plaintext.

## Edit one entry

`crypt-encrypt-entry` and `crypt-decrypt-entry` take a parsed document, a headline node from that document, and the registry. Each returns an `edit-result`; use its updated document for a later edit.

```lisp
(labels ((find-heading (document title)
           (let ((found nil))
             (org-kit:map-nodes
              (lambda (node)
                (when (and (null found)
                           (eq (org-kit:node-type node) :headline)
                           (string= (org-kit:node-property node :raw-value) title))
                  (setf found node)))
              (org-kit:document-root document))
             found)))
  (let* ((source (format nil "* Private notes~%secret text~%"))
         (document (org-kit:parse-org source))
         (heading (find-heading document "Private notes"))
         (registry (org-kit:make-callback-registry)))
    (org-kit:register-callback
     registry :crypt-encrypt "org-crypt" #'my-encrypt-function)
    (org-kit:register-callback
     registry :crypt-decrypt "org-crypt" #'my-decrypt-function)
    (let ((result (org-kit:crypt-encrypt-entry document heading registry)))
      (org-kit:document-text
       (org-kit:edit-result-updated-document result)))))
```

The operations leave the input document unchanged. `edit-result-spans` lists half-open character-offset ranges in the original document text that the operation replaced. If encryption finds an existing armor block, the entry has no body, or decryption finds no recognized armor block, the list is empty and the operation is a no-op.

For encryption, the payload starts after the metadata skipped by Org 9.8.10's `org-end-of-meta-data 'standard` boundary. That boundary skips at most one planning line immediately after the heading, then an immediately following property drawer, then repeatedly skips leading blank lines, CLOCK lines, and LOGBOOK drawers. A second planning line is body text. Arbitrary drawers after the boundary are body text. The payload continues through descendants of the entry and stops before trailing blank lines or the next heading at the same or a higher level.

The first payload line identifies encrypted content only when it is exactly `-----BEGIN PGP MESSAGE-----`, with optional leading spaces or tabs. The operation then looks for an exact `-----END PGP MESSAGE-----` line, also allowing leading spaces or tabs, before the next heading. Without that footer before the next heading, decryption does not recognize an armor block and is a no-op. An encryption callback must return only one such block: the BEGIN line is first and the END line is last, with no text outside the markers. A final line ending is allowed. This checks the armor shape, not the PGP contents. During decryption, indentation is removed from every armor line. Only the range from the BEGIN line through the END line is replaced, so text after the footer stays in place. If any plaintext heading is at the same or a shallower level than the encrypted entry, all plaintext headings shift by the same amount so the shallowest is one level below the entry; their relative levels stay the same.

## Edit tagged entries

`crypt-encrypt-entries` and `crypt-decrypt-entries` apply the operation to tagged headings and return one `edit-result` for the document. A matching heading is processed once with its subtree, so a matching descendant is not a separate target. The tag scan excludes headings whose local or inherited tags include `ARCHIVE` and excludes COMMENT subtrees as independent roots. A selected ancestor still spans its full subtree: encryption includes descendants in those regions, while decryption searches the subtree for the first armor block, including one inside those regions.

With a parsed `document` and configured `registry`, call either function:

```lisp
(org-kit:crypt-encrypt-entries document registry)
(org-kit:crypt-decrypt-entries document registry :tag "private")
```

The default tag is `"crypt"`. `:tag` accepts a string or symbol. Tag matching includes inherited tags by default; pass `:inherit nil` to match only tags written on each heading. If a callback fails or a configured resource limit is exceeded, the operation signals `org-crypt-error` or `org-resource-limit-error` without producing an edit result and leaves its input document unchanged. Side effects performed by a callback remain the caller's responsibility.

Edits are checked against `:max-output-length`, `:max-output-octets`, and `:max-input-length` (the last bounds output characters for unencoded documents and output octets for encoded documents). See [Resource limits](../reference/node-model.md#resource-limits) for units and defaults. The library does not manage cryptographic keys or temporary files.

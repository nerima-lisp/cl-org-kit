# Release Standard

`CHANGELOG.md` is the canonical changelog. Each GitHub Release body copies its
matching version section.

## Prepare a release

1. Choose the release version and set `:version` in `cl-org-kit.asd` to that
   exact value.
2. Update the matching `CHANGELOG.md` section with user-facing notes according
   to [Write and publish the notes](#write-and-publish-the-notes).
3. Run the repository gate on the release commit:

   ~~~sh
   nix flake check --print-build-logs
   ~~~

4. Push a tag named `v<version>` for that commit. The release workflow starts
   when it receives a tag matching `v*.*.*`.

## Workflow checks and draft

The workflow verifies that the tag version matches `:version` in
`cl-org-kit.asd`, then runs `nix flake check --print-build-logs` against the
tagged tree. It stops if either check fails. On success, it creates an empty
draft release.

The workflow does not read `CHANGELOG.md`, generate release notes, or publish
the release. Copy the matching version section into the draft, then publish
after reviewing the rendered notes.

## Write and publish the notes

The release notes should describe only changes visible to users of the
package. Organize them around the user impact:

- State any API or behavior change that requires caller changes, with a short
  migration instruction.
- Describe new public features and the relevant exported entry points.
- Summarize fixes that change results or remove a limitation.
- State important limits or features that remain unavailable when they affect
  upgrade decisions.

Exclude changes that only affect internal implementation, tests, or repository
maintenance. Do not rely on generated commit or pull-request summaries to
decide which changes matter to users.

Keep the release in draft while updating and reviewing its notes. Copy the
matching changelog section into the draft, inspect the rendered notes, and
publish as a separate step:

~~~sh
gh release edit v0.2.0 --notes-file release-notes.md
gh release view v0.2.0 --web
gh release edit v0.2.0 --draft=false
~~~

Replace the example tag and notes-file path with the release being prepared.
After publishing, verify the title, tag, and body in GitHub before announcing
the release.

# Release Standard

The GitHub Release description is the canonical changelog for `cl-org-kit`.
There is no separate `CHANGELOG.md`. The release workflow creates an empty
draft; a maintainer writes and reviews the notes before publishing.

## Prepare a release

1. Choose the release version and set `:version` in `cl-org-kit.asd` to that
   exact value.
2. Review the changes included in the release and draft user-facing notes
   according to the criteria in [Write and publish the notes](#write-and-publish-the-notes).
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

The workflow does not generate release notes and does not publish the release.
Do not publish the draft until its notes are complete and reviewed.

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

Keep the release in draft while updating and reviewing its notes. First write
the notes, then inspect the rendered draft, and publish it as a separate step:

~~~sh
gh release edit v0.1.0 --notes-file release-notes.md
gh release view v0.1.0 --web
gh release edit v0.1.0 --draft=false
~~~

Replace the example tag and notes-file path with the release being prepared.
After publishing, verify the title, tag, and body in GitHub before announcing
the release.

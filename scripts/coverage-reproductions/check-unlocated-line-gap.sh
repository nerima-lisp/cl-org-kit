#!/bin/sh
set -eu

reproduction_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repository_root=$(CDPATH= cd -- "$reproduction_dir/../.." && pwd)
checker=${1:-$repository_root/scripts/check-coverage.pl}
output=$(mktemp)
trap 'rm -f "$output"' EXIT HUP INT TERM

set +e
perl "$checker" \
  --input "$reproduction_dir/unlocated-line-gap-per-file.tsv" \
  --source-root "$reproduction_dir" \
  --exclusions "$repository_root/scripts/coverage-exclusions.sexp" \
  >"$output" 2>&1
status=$?
set -e

if [ "$status" -ne 1 ]; then
  cat "$output"
  printf 'expected checker exit 1, got %s\n' "$status" >&2
  exit 1
fi
if ! grep -Fq '| branch-gap-source.lisp | 0.0% | 100.0% | - |' "$output"; then
  cat "$output"
  printf 'expected an unlocated line coverage deficit in the report\n' >&2
  exit 1
fi
cat "$output"

#!/bin/sh
set -eu

reproduction_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repository_root=$(CDPATH= cd -- "$reproduction_dir/../.." && pwd)
checker=${1:-$repository_root/scripts/check-coverage.pl}
expected_status=${2:-1}
output=$(mktemp)
trap 'rm -f "$output"' EXIT HUP INT TERM

set +e
perl "$checker" \
  --input "$reproduction_dir/branch-gap-per-file.tsv" \
  --source-root "$reproduction_dir" \
  --exclusions "$repository_root/scripts/coverage-exclusions.sexp" \
  >"$output" 2>&1
status=$?
set -e

if [ "$status" -ne "$expected_status" ]; then
  cat "$output"
  printf 'expected checker exit %s, got %s\n' "$expected_status" "$status" >&2
  exit 1
fi
cat "$output"

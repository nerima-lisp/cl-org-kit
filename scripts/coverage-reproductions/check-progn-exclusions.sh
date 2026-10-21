#!/bin/sh
set -eu

reproduction_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repository_root=$(CDPATH= cd -- "$reproduction_dir/../.." && pwd)
checker=${1:-$repository_root/scripts/check-coverage.pl}
output_dir=$(mktemp -d)
trap 'rm -f "$output_dir/progn-direct.lisp.out" "$output_dir/progn-nested.lisp.out"; rmdir "$output_dir"' EXIT HUP INT TERM

run_checker() {
  source_name=$1
  summary_name=$2
  expected_status=$3
  output="$output_dir/$source_name.out"

  set +e
  perl "$checker" \
    --input "$reproduction_dir/$summary_name" \
    --source-root "$reproduction_dir" \
    --exclusions "$repository_root/scripts/coverage-exclusions.sexp" \
    >"$output" 2>&1
  status=$?
  set -e

  if [ "$status" -ne "$expected_status" ]; then
    cat "$output"
    printf 'expected checker exit %s for %s, got %s\n' \
      "$expected_status" "$source_name" "$status" >&2
    exit 1
  fi
}

run_checker progn-direct.lisp progn-direct-per-file.tsv 0
if ! grep -Fq '| progn-direct.lisp | 0.0% | 100.0% | 2 |' "$output_dir/progn-direct.lisp.out" \
    || ! grep -Fq 'coverage gate: PASS (all 1 files at or above threshold)' "$output_dir/progn-direct.lisp.out"; then
  cat "$output_dir/progn-direct.lisp.out"
  printf 'expected the direct progn body exclusion to pass the coverage gate\n' >&2
  exit 1
fi

run_checker progn-nested.lisp progn-nested-per-file.tsv 1
if ! grep -Fq '| progn-nested.lisp | 0.0% | 100.0% | 3 |' "$output_dir/progn-nested.lisp.out" \
    || ! grep -Fq 'progn-nested.lisp: line 0.0%, required 100.0%' "$output_dir/progn-nested.lisp.out" \
    || ! grep -Fq 'coverage gate: FAIL (1 of 1 files below threshold)' "$output_dir/progn-nested.lisp.out"; then
  cat "$output_dir/progn-nested.lisp.out"
  printf 'expected the nested progn form to remain uncovered\n' >&2
  exit 1
fi

cat "$output_dir/progn-direct.lisp.out" "$output_dir/progn-nested.lisp.out"

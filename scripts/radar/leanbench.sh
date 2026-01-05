#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 ]]; then
  echo "usage: $0 <repo-path> <radar-jsonl-out> [leanbench args...]" >&2
  exit 2
fi

repo_path="$1"
out_path="$2"
shift 2

bench_args=("$@")
if [[ -n "${LEANBENCH_ARGS:-}" ]]; then
  # Allow space-separated overrides from env for radar config.
  read -r -a extra_args <<< "${LEANBENCH_ARGS}"
  bench_args+=("${extra_args[@]}")
fi

cd "$repo_path"

lake exe leanbench --radar --radar-out "$out_path" "${bench_args[@]}"
